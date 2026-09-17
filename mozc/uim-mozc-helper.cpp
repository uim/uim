/*
  Copyright (c) 2026 uim Project https://github.com/uim/uim

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
  3. Neither the name of authors nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.
*/

/*
  Parts of this file are derived from Mozc (https://github.com/google/mozc):

    Copyright 2010-2021, Google Inc.
    All rights reserved.

  Specifically:
    - the protobuf-to-S-expression printer (PrintMessage, PrintField,
      PrintFieldValue, NormalizeSymbol, QuoteString) is derived from
      src/unix/emacs/mozc_emacs_helper_lib.cc;
    - GetUserProfileDirectory() is derived from base/system_util.cc;
    - IsValidKey() and the IPC socket path/name derivation are derived
      from ipc/ipc_path_manager.cc;
    - the request framing (one connection per request, shutdown(SHUT_WR),
      read until EOF, SO_PEERCRED uid check) follows ipc/unix_ipc.cc;
    - the candidate-window usage-data stripping in Response() follows
      RemoveUsageData() in mozc_emacs_helper_lib.cc;
    - the CREATE_SESSION request setup (Capability, ApplicationInfo)
      follows Mozc's client conventions (client/client.cc).

  Mozc is BSD-3-Clause licensed; these parts are used here under the same
  terms reproduced above. The S-expression reader, the alist-to-protobuf
  filler (FillMessage), the ServerKeeper/Bridge logic and the protocol
  with uim are not from Mozc.
*/

/*
 * uim-mozc-helper: a bridge between uim and mozc_server.
 *
 * See README.md in this directory for the protocol.
 */

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <pwd.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <unistd.h>

#include <cstdint>
#include <fstream>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

#include <google/protobuf/descriptor.h>
#include <google/protobuf/message.h>

#include "ipc/ipc.pb.h"
#include "protocol/commands.pb.h"
#include "protocol/config.pb.h"

namespace {

namespace pb = google::protobuf;
using mozc::commands::Input;
using mozc::commands::KeyEvent;
using mozc::commands::Output;
using mozc::commands::SessionCommand;

const char *kProgramName = "uim-mozc-helper";
const char *kSessionName = "session";
const int kIPCTimeoutSeconds = 10;
const int kServerWaitSeconds = 15;

/* ------------------------------------------------------------------ */
/* S-expression values                                                 */

struct Value;
typedef std::shared_ptr<Value> ValuePtr;

struct Value {
  enum Kind { NIL, INT, STR, SYM, CONS };
  Kind kind;
  int64_t i;
  std::string s;
  ValuePtr car;
  ValuePtr cdr;

  explicit Value(Kind k) : kind(k), i(0) {}
};

ValuePtr MakeNil() { return std::make_shared<Value>(Value::NIL); }
ValuePtr MakeInt(int64_t i) {
  ValuePtr v = std::make_shared<Value>(Value::INT);
  v->i = i;
  return v;
}
ValuePtr MakeStr(const std::string &s) {
  ValuePtr v = std::make_shared<Value>(Value::STR);
  v->s = s;
  return v;
}
ValuePtr MakeSym(const std::string &s) {
  ValuePtr v = std::make_shared<Value>(Value::SYM);
  v->s = s;
  return v;
}
ValuePtr Cons(ValuePtr car, ValuePtr cdr) {
  ValuePtr v = std::make_shared<Value>(Value::CONS);
  v->car = car;
  v->cdr = cdr;
  return v;
}

bool IsNil(const ValuePtr &v) { return !v || v->kind == Value::NIL; }
bool IsCons(const ValuePtr &v) { return v && v->kind == Value::CONS; }
bool IsSym(const ValuePtr &v, const char *name) {
  return v && v->kind == Value::SYM && v->s == name;
}

/* Returns the elements of a proper list. */
std::vector<ValuePtr> ListToVector(ValuePtr v) {
  std::vector<ValuePtr> result;
  while (IsCons(v)) {
    result.push_back(v->car);
    v = v->cdr;
  }
  return result;
}

/* Looks up KEY in an alist. Returns the cdr of the matching pair. */
ValuePtr AlistGet(const ValuePtr &alist, const char *key, bool *found) {
  *found = false;
  ValuePtr v = alist;
  while (IsCons(v)) {
    if (IsCons(v->car) && IsSym(v->car->car, key)) {
      *found = true;
      return v->car->cdr;
    }
    v = v->cdr;
  }
  return ValuePtr();
}

/* ------------------------------------------------------------------ */
/* S-expression reader                                                 */

class Reader {
 public:
  explicit Reader(const std::string &input) : input_(input), pos_(0) {}

  bool Read(ValuePtr *out, std::string *error) {
    SkipSpaces();
    if (pos_ >= input_.size()) {
      *error = "empty input";
      return false;
    }
    return ReadValue(out, error);
  }

 private:
  void SkipSpaces() {
    while (pos_ < input_.size() && isspace((unsigned char)input_[pos_])) {
      pos_++;
    }
  }

  static bool IsDelimiter(char c) {
    return isspace((unsigned char)c) || c == '(' || c == ')' || c == '"';
  }

  bool ReadValue(ValuePtr *out, std::string *error) {
    SkipSpaces();
    if (pos_ >= input_.size()) {
      *error = "unexpected end of input";
      return false;
    }
    char c = input_[pos_];
    if (c == '(') {
      pos_++;
      return ReadList(out, error);
    }
    if (c == ')') {
      *error = "unexpected ')'";
      return false;
    }
    if (c == '"') {
      pos_++;
      return ReadString(out, error);
    }
    return ReadAtom(out, error);
  }

  bool ReadList(ValuePtr *out, std::string *error) {
    std::vector<ValuePtr> items;
    ValuePtr tail = MakeNil();
    while (true) {
      SkipSpaces();
      if (pos_ >= input_.size()) {
        *error = "unterminated list";
        return false;
      }
      char c = input_[pos_];
      if (c == ')') {
        pos_++;
        break;
      }
      if (c == '.' && pos_ + 1 < input_.size() &&
          IsDelimiter(input_[pos_ + 1]) && !items.empty()) {
        pos_++;
        if (!ReadValue(&tail, error)) {
          return false;
        }
        SkipSpaces();
        if (pos_ >= input_.size() || input_[pos_] != ')') {
          *error = "expected ')' after dotted tail";
          return false;
        }
        pos_++;
        break;
      }
      ValuePtr item;
      if (!ReadValue(&item, error)) {
        return false;
      }
      items.push_back(item);
    }
    ValuePtr result = tail;
    for (size_t i = items.size(); i > 0; i--) {
      result = Cons(items[i - 1], result);
    }
    *out = result;
    return true;
  }

  bool ReadString(ValuePtr *out, std::string *error) {
    std::string s;
    while (true) {
      if (pos_ >= input_.size()) {
        *error = "unterminated string";
        return false;
      }
      char c = input_[pos_++];
      if (c == '"') {
        break;
      }
      if (c == '\\') {
        if (pos_ >= input_.size()) {
          *error = "unterminated string";
          return false;
        }
        char e = input_[pos_++];
        switch (e) {
          case 'n': s += '\n'; break;
          case 'r': s += '\r'; break;
          case 't': s += '\t'; break;
          default: s += e; break;
        }
      } else {
        s += c;
      }
    }
    *out = MakeStr(s);
    return true;
  }

  bool ReadAtom(ValuePtr *out, std::string *error) {
    size_t start = pos_;
    while (pos_ < input_.size() && !IsDelimiter(input_[pos_])) {
      pos_++;
    }
    std::string token = input_.substr(start, pos_ - start);
    if (token.empty()) {
      *error = "empty token";
      return false;
    }
    bool is_number = true;
    size_t i = 0;
    if (token[0] == '-' || token[0] == '+') {
      i = 1;
      if (token.size() == 1) {
        is_number = false;
      }
    }
    for (; i < token.size(); i++) {
      if (!isdigit((unsigned char)token[i])) {
        is_number = false;
        break;
      }
    }
    if (is_number) {
      *out = MakeInt(strtoll(token.c_str(), NULL, 10));
    } else {
      *out = MakeSym(token);
    }
    return true;
  }

  const std::string input_;
  size_t pos_;
};

/* ------------------------------------------------------------------ */
/* S-expression printer (mozc_emacs_helper compatible)                 */

std::string NormalizeSymbol(std::string_view symbol) {
  std::string normalized(symbol);
  for (size_t i = 0; i < normalized.size(); i++) {
    char c = normalized[i];
    if (c == '_') {
      normalized[i] = '-';
    } else {
      normalized[i] = static_cast<char>(tolower((unsigned char)c));
    }
  }
  return normalized;
}

std::string DenormalizeSymbol(const std::string &symbol) {
  std::string denormalized(symbol);
  for (size_t i = 0; i < denormalized.size(); i++) {
    char c = denormalized[i];
    if (c == '-') {
      denormalized[i] = '_';
    } else {
      denormalized[i] = static_cast<char>(toupper((unsigned char)c));
    }
  }
  return denormalized;
}

std::string QuoteString(std::string_view str) {
  std::string quoted = "\"";
  for (size_t i = 0; i < str.size(); i++) {
    char c = str[i];
    switch (c) {
      case '\\': quoted += "\\\\"; break;
      case '"': quoted += "\\\""; break;
      case '\n': quoted += "\\n"; break;
      case '\r': quoted += "\\r"; break;
      default: quoted += c; break;
    }
  }
  quoted += "\"";
  return quoted;
}

/*
 * The following S-expression printer (PrintFieldValue, PrintField,
 * PrintMessage) is derived from Mozc's mozc_emacs_helper_lib.cc
 * (Copyright Google Inc., BSD-3-Clause); see the file header.
 */
void PrintMessage(const pb::Message &message, std::string *output);

void PrintFieldValue(const pb::Message &message,
                     const pb::Reflection &reflection,
                     const pb::FieldDescriptor &field,
                     int index,
                     std::string *output) {
#define GET_FIELD_VALUE(METHOD_TYPE)                                     \
  (field.is_repeated()                                                   \
       ? reflection.GetRepeated##METHOD_TYPE(message, &field, index)     \
       : reflection.Get##METHOD_TYPE(message, &field))

  switch (field.cpp_type()) {
    case pb::FieldDescriptor::CPPTYPE_INT32:
      *output += std::to_string(GET_FIELD_VALUE(Int32));
      break;
    case pb::FieldDescriptor::CPPTYPE_UINT32:
      *output += std::to_string(GET_FIELD_VALUE(UInt32));
      break;
    /* 64-bit integers are printed as strings like mozc_emacs_helper. */
    case pb::FieldDescriptor::CPPTYPE_INT64:
      *output += "\"" + std::to_string(GET_FIELD_VALUE(Int64)) + "\"";
      break;
    case pb::FieldDescriptor::CPPTYPE_UINT64:
      *output += "\"" + std::to_string(GET_FIELD_VALUE(UInt64)) + "\"";
      break;
    case pb::FieldDescriptor::CPPTYPE_DOUBLE:
      *output += std::to_string(GET_FIELD_VALUE(Double));
      break;
    case pb::FieldDescriptor::CPPTYPE_FLOAT:
      *output += std::to_string(GET_FIELD_VALUE(Float));
      break;
    case pb::FieldDescriptor::CPPTYPE_BOOL:
      /* Scheme booleans: the reader in mozc.scm treats the symbol `nil`
       * as true, so false must be #f, not nil. */
      *output += GET_FIELD_VALUE(Bool) ? "#t" : "#f";
      break;
    case pb::FieldDescriptor::CPPTYPE_ENUM:
      *output += NormalizeSymbol(GET_FIELD_VALUE(Enum)->name());
      break;
    case pb::FieldDescriptor::CPPTYPE_STRING: {
      std::string scratch;
      std::string_view str =
          field.is_repeated()
              ? reflection.GetRepeatedStringReference(message, &field, index,
                                                      &scratch)
              : reflection.GetStringReference(message, &field, &scratch);
      *output += QuoteString(str);
      break;
    }
    case pb::FieldDescriptor::CPPTYPE_MESSAGE:
      PrintMessage(GET_FIELD_VALUE(Message), output);
      break;
  }
#undef GET_FIELD_VALUE
}

void PrintField(const pb::Message &message,
                const pb::Reflection &reflection,
                const pb::FieldDescriptor &field,
                std::string *output) {
  *output += "(";
  *output += NormalizeSymbol(field.name());
  if (!field.is_repeated()) {
    *output += " . ";
    PrintFieldValue(message, reflection, field, -1, output);
  } else {
    *output += " ";
    const int count = reflection.FieldSize(message, &field);
    const bool is_message =
        field.cpp_type() == pb::FieldDescriptor::CPPTYPE_MESSAGE;
    for (int i = 0; i < count; i++) {
      if (i != 0 && !is_message) {
        *output += " ";
      }
      PrintFieldValue(message, reflection, field, i, output);
    }
  }
  *output += ")";
}

void PrintMessage(const pb::Message &message, std::string *output) {
  const pb::Reflection *reflection = message.GetReflection();
  std::vector<const pb::FieldDescriptor *> fields;
  reflection->ListFields(message, &fields);
  *output += "(";
  for (size_t i = 0; i < fields.size(); i++) {
    PrintField(message, *reflection, *fields[i], output);
  }
  *output += ")";
}

/* ------------------------------------------------------------------ */
/* S-expression alist -> protocol buffer message                       */

bool FillMessage(const ValuePtr &alist, pb::Message *message,
                 std::string *error);

bool SetFieldValue(const ValuePtr &value,
                   pb::Message *message,
                   const pb::Reflection &reflection,
                   const pb::FieldDescriptor &field,
                   std::string *error) {
  const bool repeated = field.is_repeated();
  const std::string name(field.full_name());
  switch (field.cpp_type()) {
    case pb::FieldDescriptor::CPPTYPE_INT32:
    case pb::FieldDescriptor::CPPTYPE_UINT32:
    case pb::FieldDescriptor::CPPTYPE_INT64:
    case pb::FieldDescriptor::CPPTYPE_UINT64: {
      int64_t n = 0;
      if (value && value->kind == Value::INT) {
        n = value->i;
      } else if (value && value->kind == Value::STR) {
        n = strtoll(value->s.c_str(), NULL, 10);
      } else {
        *error = name + ": integer is expected";
        return false;
      }
      switch (field.cpp_type()) {
        case pb::FieldDescriptor::CPPTYPE_INT32:
          repeated ? reflection.AddInt32(message, &field, (int32_t)n)
                   : reflection.SetInt32(message, &field, (int32_t)n);
          break;
        case pb::FieldDescriptor::CPPTYPE_UINT32:
          repeated ? reflection.AddUInt32(message, &field, (uint32_t)n)
                   : reflection.SetUInt32(message, &field, (uint32_t)n);
          break;
        case pb::FieldDescriptor::CPPTYPE_INT64:
          repeated ? reflection.AddInt64(message, &field, n)
                   : reflection.SetInt64(message, &field, n);
          break;
        default:
          repeated ? reflection.AddUInt64(message, &field, (uint64_t)n)
                   : reflection.SetUInt64(message, &field, (uint64_t)n);
          break;
      }
      return true;
    }
    case pb::FieldDescriptor::CPPTYPE_DOUBLE:
    case pb::FieldDescriptor::CPPTYPE_FLOAT: {
      double d = 0;
      if (value && value->kind == Value::INT) {
        d = (double)value->i;
      } else if (value && value->kind == Value::STR) {
        d = strtod(value->s.c_str(), NULL);
      } else if (value && value->kind == Value::SYM) {
        d = strtod(value->s.c_str(), NULL);
      } else {
        *error = name + ": number is expected";
        return false;
      }
      if (field.cpp_type() == pb::FieldDescriptor::CPPTYPE_DOUBLE) {
        repeated ? reflection.AddDouble(message, &field, d)
                 : reflection.SetDouble(message, &field, d);
      } else {
        repeated ? reflection.AddFloat(message, &field, (float)d)
                 : reflection.SetFloat(message, &field, (float)d);
      }
      return true;
    }
    case pb::FieldDescriptor::CPPTYPE_BOOL: {
      bool b;
      if (IsNil(value) || IsSym(value, "nil") || IsSym(value, "#f")) {
        b = false;
      } else if (value->kind == Value::INT) {
        b = value->i != 0;
      } else {
        b = true;
      }
      repeated ? reflection.AddBool(message, &field, b)
               : reflection.SetBool(message, &field, b);
      return true;
    }
    case pb::FieldDescriptor::CPPTYPE_ENUM: {
      const pb::EnumValueDescriptor *enum_value = NULL;
      if (value && value->kind == Value::INT) {
        enum_value = field.enum_type()->FindValueByNumber((int)value->i);
      } else if (value && (value->kind == Value::SYM ||
                           value->kind == Value::STR)) {
        enum_value = field.enum_type()->FindValueByName(
            DenormalizeSymbol(value->s));
        if (!enum_value) {
          enum_value = field.enum_type()->FindValueByName(value->s);
        }
      }
      if (!enum_value) {
        *error = name + ": unknown enum value";
        return false;
      }
      repeated ? reflection.AddEnum(message, &field, enum_value)
               : reflection.SetEnum(message, &field, enum_value);
      return true;
    }
    case pb::FieldDescriptor::CPPTYPE_STRING: {
      std::string s;
      if (value && (value->kind == Value::STR || value->kind == Value::SYM)) {
        s = value->s;
      } else if (value && value->kind == Value::INT) {
        s = std::to_string(value->i);
      } else {
        *error = name + ": string is expected";
        return false;
      }
      repeated ? reflection.AddString(message, &field, s)
               : reflection.SetString(message, &field, s);
      return true;
    }
    case pb::FieldDescriptor::CPPTYPE_MESSAGE: {
      pb::Message *sub = repeated
                             ? reflection.AddMessage(message, &field)
                             : reflection.MutableMessage(message, &field);
      return FillMessage(value, sub, error);
    }
  }
  *error = name + ": unsupported field type";
  return false;
}

/*
 * ALIST is a list of entries. An entry is either
 *   (field-name . value)          for a non repeated field, or
 *   (field-name value1 value2...) for a repeated field.
 */
bool FillMessage(const ValuePtr &alist, pb::Message *message,
                 std::string *error) {
  const pb::Descriptor *descriptor = message->GetDescriptor();
  const pb::Reflection *reflection = message->GetReflection();
  ValuePtr entry = alist;
  while (IsCons(entry)) {
    const ValuePtr &pair = entry->car;
    entry = entry->cdr;
    if (!IsCons(pair) || !pair->car || pair->car->kind != Value::SYM) {
      *error = std::string(descriptor->full_name()) + ": alist entry is expected";
      return false;
    }
    std::string field_name = pair->car->s;
    for (size_t i = 0; i < field_name.size(); i++) {
      if (field_name[i] == '-') {
        field_name[i] = '_';
      }
    }
    const pb::FieldDescriptor *field =
        descriptor->FindFieldByName(field_name);
    if (!field) {
      *error = std::string(descriptor->full_name()) + ": unknown field: " + pair->car->s;
      return false;
    }
    if (field->is_repeated()) {
      std::vector<ValuePtr> values = ListToVector(pair->cdr);
      for (size_t i = 0; i < values.size(); i++) {
        if (!SetFieldValue(values[i], message, *reflection, *field, error)) {
          return false;
        }
      }
    } else {
      if (!SetFieldValue(pair->cdr, message, *reflection, *field, error)) {
        return false;
      }
    }
  }
  return true;
}

/* ------------------------------------------------------------------ */
/* Logging                                                             */

void Warn(const std::string &message) {
  fprintf(stderr, "%s: %s\n", kProgramName, message.c_str());
  fflush(stderr);
}

/* ------------------------------------------------------------------ */
/* Server keeper: mozc_emacs_helper                                    */

/*
 * mozc_emacs_helper links the official Mozc client library. It knows
 * where mozc_server is, launches it and handles version mismatches.
 * We use it only for that purpose.
 */
class ServerKeeper {
 public:
  explicit ServerKeeper(const std::string &command)
      : command_(command), pid_(0), in_(NULL), out_(NULL), event_id_(0) {}

  ~ServerKeeper() { Stop(); }

  bool enabled() const { return !command_.empty(); }
  const std::string &version() const { return version_; }
  const std::string &preedit_method() const { return preedit_method_; }

  /* Makes sure that mozc_server is running. */
  bool EnsureServer() {
    if (!enabled()) {
      return false;
    }
    if (pid_ == 0 && !Start()) {
      return false;
    }
    /*
     * CreateSession forces the Mozc client in mozc_emacs_helper to
     * connect to mozc_server, launching it if necessary.
     */
    std::string response;
    if (!Request("CreateSession", &response)) {
      /* mozc_emacs_helper exits on any error. Retry once. */
      Stop();
      if (!Start() || !Request("CreateSession", &response)) {
        return false;
      }
    }
    std::string error;
    ValuePtr value;
    Reader reader(response);
    if (!reader.Read(&value, &error)) {
      Warn("broken response from " + command_ + ": " + error);
      return false;
    }
    bool found = false;
    /* mozc_emacs_helper's own response really uses "emacs-session-id". */
    ValuePtr session_id = AlistGet(value, "emacs-session-id", &found);
    if (found && session_id && session_id->kind == Value::INT) {
      std::string ignored;
      Request("DeleteSession " + std::to_string(session_id->i), &ignored);
    }
    return true;
  }

 private:
  bool Start() {
    int to_child[2];
    int from_child[2];
    if (pipe(to_child) != 0) {
      Warn(std::string("pipe() failed: ") + strerror(errno));
      return false;
    }
    if (pipe(from_child) != 0) {
      Warn(std::string("pipe() failed: ") + strerror(errno));
      close(to_child[0]);
      close(to_child[1]);
      return false;
    }
    pid_t pid = fork();
    if (pid < 0) {
      Warn(std::string("fork() failed: ") + strerror(errno));
      close(to_child[0]);
      close(to_child[1]);
      close(from_child[0]);
      close(from_child[1]);
      return false;
    }
    if (pid == 0) {
      dup2(to_child[0], 0);
      dup2(from_child[1], 1);
      close(to_child[0]);
      close(to_child[1]);
      close(from_child[0]);
      close(from_child[1]);
      execlp(command_.c_str(), command_.c_str(), (char *)NULL);
      fprintf(stderr, "%s: failed to execute %s: %s\n",
              kProgramName, command_.c_str(), strerror(errno));
      _exit(127);
    }
    close(to_child[0]);
    close(from_child[1]);
    pid_ = pid;
    out_ = fdopen(to_child[1], "w");
    in_ = fdopen(from_child[0], "r");
    event_id_ = 0;

    std::string greeting;
    if (!ReadLine(&greeting)) {
      Warn("failed to read greeting from " + command_);
      Stop();
      return false;
    }
    ParseGreeting(greeting);
    return true;
  }

  void Stop() {
    if (out_) {
      fclose(out_);
      out_ = NULL;
    }
    if (in_) {
      fclose(in_);
      in_ = NULL;
    }
    if (pid_ > 0) {
      int status;
      waitpid(pid_, &status, 0);
      pid_ = 0;
    }
  }

  bool ReadLine(std::string *line) {
    line->clear();
    if (!in_) {
      return false;
    }
    int c;
    while ((c = fgetc(in_)) != EOF) {
      if (c == '\n') {
        return true;
      }
      *line += static_cast<char>(c);
    }
    return !line->empty();
  }

  bool Request(const std::string &command, std::string *response) {
    if (!out_ || !in_) {
      return false;
    }
    fprintf(out_, "(%u %s)\n", event_id_++, command.c_str());
    if (fflush(out_) != 0) {
      return false;
    }
    return ReadLine(response);
  }

  void ParseGreeting(const std::string &greeting) {
    std::string error;
    ValuePtr value;
    Reader reader(greeting);
    if (!reader.Read(&value, &error)) {
      return;
    }
    bool found;
    ValuePtr version = AlistGet(value, "version", &found);
    if (found && version && version->kind == Value::STR) {
      version_ = version->s;
    }
    ValuePtr config = AlistGet(value, "config", &found);
    if (found) {
      ValuePtr method = AlistGet(config, "preedit-method", &found);
      if (found && method && method->kind == Value::SYM) {
        preedit_method_ = method->s;
      }
    }
  }

  std::string command_;
  pid_t pid_;
  FILE *in_;
  FILE *out_;
  unsigned int event_id_;
  std::string version_;
  std::string preedit_method_;
};

/* ------------------------------------------------------------------ */
/* Direct IPC with mozc_server                                         */

std::string JoinPath(const std::string &dir, const std::string &name) {
  if (dir.empty() || dir[dir.size() - 1] == '/') {
    return dir + name;
  }
  return dir + "/" + name;
}

bool DirectoryExists(const std::string &path) {
  struct stat st;
  return stat(path.c_str(), &st) == 0 && S_ISDIR(st.st_mode);
}

/* Same logic as SystemUtil::GetUserProfileDirectory() on Linux. */
std::string GetUserProfileDirectory() {
  const char *home_env = getenv("HOME");
  std::string home = home_env ? home_env : "";
  if (home.empty()) {
    struct passwd *pw = getpwuid(geteuid());
    if (pw && pw->pw_dir) {
      return JoinPath(pw->pw_dir, ".mozc");
    }
    return "";
  }
  std::string old_dir = JoinPath(home, ".mozc");
  if (DirectoryExists(old_dir)) {
    return old_dir;
  }
  const char *xdg = getenv("XDG_CONFIG_HOME");
  if (xdg && *xdg) {
    return JoinPath(xdg, "mozc");
  }
  return JoinPath(home, ".config/mozc");
}

bool IsValidKey(const std::string &key) {
  if (key.size() != 32) {
    return false;
  }
  for (size_t i = 0; i < key.size(); i++) {
    char c = key[i];
    if (!((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))) {
      return false;
    }
  }
  return true;
}

class IPCClient {
 public:
  IPCClient() {}

  /* Reads ~/.mozc/.session.ipc (or the XDG equivalent). */
  bool LoadPathInfo(std::string *error) {
    std::string dir = GetUserProfileDirectory();
    if (dir.empty()) {
      *error = "cannot determine Mozc user profile directory";
      return false;
    }
    std::string filename = JoinPath(dir, std::string(".") + kSessionName +
                                             ".ipc");
    std::ifstream is(filename.c_str(), std::ios::binary | std::ios::in);
    if (!is) {
      *error = "cannot open " + filename;
      return false;
    }
    mozc::ipc::IPCPathInfo info;
    if (!info.ParseFromIstream(&is)) {
      *error = "cannot parse " + filename;
      return false;
    }
    if (!IsValidKey(info.key())) {
      *error = "invalid IPC key in " + filename;
      return false;
    }
    /*
     * Same as IPCPathManager::GetPathName() on Linux: the leading
     * '/' is replaced with NUL so that the socket is in the abstract
     * namespace.
     */
    socket_name_ = "/tmp/.mozc.";
    socket_name_[0] = '\0';
    socket_name_ += info.key();
    socket_name_ += ".";
    socket_name_ += kSessionName;
    protocol_version_ = info.protocol_version();
    product_version_ = info.product_version();
    return true;
  }

  bool loaded() const { return !socket_name_.empty(); }
  const std::string &product_version() const { return product_version_; }
  uint32_t protocol_version() const { return protocol_version_; }

  /* One request per connection, like Mozc's IPCClient::Call(). */
  bool Call(const Input &input, Output *output, std::string *error) {
    if (!loaded()) {
      *error = "IPC path is not loaded";
      return false;
    }
    int sock = socket(PF_UNIX, SOCK_STREAM, 0);
    if (sock < 0) {
      *error = std::string("socket() failed: ") + strerror(errno);
      return false;
    }
    fcntl(sock, F_SETFD, FD_CLOEXEC);
    struct timeval timeout;
    timeout.tv_sec = kIPCTimeoutSeconds;
    timeout.tv_usec = 0;
    setsockopt(sock, SOL_SOCKET, SO_SNDTIMEO, &timeout, sizeof(timeout));
    setsockopt(sock, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));

    struct sockaddr_un address;
    memset(&address, 0, sizeof(address));
    address.sun_family = AF_UNIX;
    size_t length = socket_name_.size();
    if (length >= sizeof(address.sun_path)) {
      length = sizeof(address.sun_path) - 1;
    }
    memcpy(address.sun_path, socket_name_.data(), length);
    socklen_t address_length =
        static_cast<socklen_t>(offsetof(struct sockaddr_un, sun_path) + length);
    if (connect(sock, (struct sockaddr *)&address, address_length) != 0) {
      *error = std::string("connect() failed: ") + strerror(errno);
      close(sock);
      return false;
    }

    struct ucred peer;
    socklen_t peer_length = sizeof(peer);
    if (getsockopt(sock, SOL_SOCKET, SO_PEERCRED, &peer, &peer_length) < 0) {
      *error = "cannot get peer credential";
      close(sock);
      return false;
    }
    if (peer.uid != geteuid()) {
      *error = "uid mismatch with mozc_server";
      close(sock);
      return false;
    }

    std::string request;
    if (!input.SerializeToString(&request)) {
      *error = "cannot serialize request";
      close(sock);
      return false;
    }
    size_t offset = 0;
    while (offset < request.size()) {
      ssize_t n = send(sock, request.data() + offset, request.size() - offset,
                       MSG_NOSIGNAL);
      if (n < 0) {
        *error = std::string("send() failed: ") + strerror(errno);
        close(sock);
        return false;
      }
      offset += n;
    }
    /* Signals the end of the request. mozc_server reads until EOF. */
    shutdown(sock, SHUT_WR);

    std::string response;
    char buffer[65536];
    while (true) {
      ssize_t n = recv(sock, buffer, sizeof(buffer), 0);
      if (n < 0) {
        *error = std::string("recv() failed: ") + strerror(errno);
        close(sock);
        return false;
      }
      if (n == 0) {
        break;
      }
      response.append(buffer, n);
    }
    close(sock);

    if (!output->ParseFromString(response)) {
      *error = "cannot parse response";
      return false;
    }
    return true;
  }

 private:
  std::string socket_name_;
  uint32_t protocol_version_ = 0;
  std::string product_version_;
};

/* ------------------------------------------------------------------ */
/* Sessions                                                            */

class Bridge {
 public:
  Bridge(const std::string &emacs_helper_command)
      : keeper_(emacs_helper_command), next_session_id_(1),
        initialized_(false) {}

  /*
   * Connects to mozc_server, launching it through the keeper if
   * needed. Called lazily on the first request so that this process
   * stays silent on stdout until then. process-io in uim uses the
   * child's early stdout to detect exec failures, so an unsolicited
   * greeting would confuse it.
   */
  bool EnsureInit(std::string *error) {
    if (initialized_) {
      return true;
    }
    if (!keeper_.EnsureServer()) {
      if (keeper_.enabled()) {
        Warn("mozc_emacs_helper isn't usable; "
             "assuming mozc_server is already running");
      }
    }
    if (!WaitForServer(error)) {
      return false;
    }
    initialized_ = true;
    return true;
  }

  std::string GreetingResponse(int64_t event_id) {
    std::string version = keeper_.version();
    if (version.empty()) {
      version = ipc_.product_version();
    }
    std::string preedit_method = keeper_.preedit_method();
    {
      Input input;
      input.set_type(Input::GET_CONFIG);
      Output output;
      std::string error;
      if (Call(input, &output, &error) && output.has_config() &&
          output.config().has_preedit_method()) {
        preedit_method = NormalizeSymbol(
            mozc::config::Config::PreeditMethod_Name(
                output.config().preedit_method()));
      }
    }
    if (preedit_method.empty()) {
      preedit_method = "roman";
    }
    std::string greeting = "((event-id . ";
    greeting += std::to_string(event_id);
    greeting += ")(version . " + QuoteString(version) + ")";
    greeting += "(config . ((preedit-method . " + preedit_method + ")))";
    greeting += "(uim-mozc-helper . t)";
#ifdef UIM_VERSION
    greeting += "(uim-version . " + QuoteString(UIM_VERSION) + ")";
#endif
    greeting += ")";
    return greeting;
  }

  /* Returns the response line for a request line. */
  std::string Process(const std::string &line) {
    std::string error;
    ValuePtr request;
    Reader reader(line);
    if (!reader.Read(&request, &error)) {
      return ErrorResponse(0, "wrong-format", error);
    }
    std::vector<ValuePtr> items = ListToVector(request);
    if (items.size() < 2 || !items[0] || items[0]->kind != Value::INT ||
        !items[1] || items[1]->kind != Value::SYM) {
      return ErrorResponse(0, "wrong-format",
                           "(EVENT_ID COMMAND ...) is expected");
    }
    const int64_t event_id = items[0]->i;
    const std::string &command = items[1]->s;

    if (command == "Hello") {
      std::string error;
      if (!EnsureInit(&error)) {
        return ErrorResponse(event_id, "no-server", error);
      }
      return GreetingResponse(event_id);
    }

    {
      std::string error;
      if (!EnsureInit(&error)) {
        return ErrorResponse(event_id, "no-server", error);
      }
    }

    if (command == "CreateSession") {
      uint64_t server_id;
      if (!CreateSession(&server_id, &error)) {
        return ErrorResponse(event_id, "session-failure", error);
      }
      uint32_t session_id = next_session_id_++;
      sessions_[session_id] = server_id;
      return Response(event_id, session_id, last_output_);
    }

    if (items.size() < 3 || !items[2] || items[2]->kind != Value::INT) {
      return ErrorResponse(event_id, "wrong-format",
                           "SESSION_ID is expected");
    }
    const uint32_t session_id = (uint32_t)items[2]->i;
    std::map<uint32_t, uint64_t>::iterator it = sessions_.find(session_id);
    if (it == sessions_.end()) {
      return ErrorResponse(event_id, "invalid-session",
                           "unknown session: " + std::to_string(session_id));
    }

    if (command == "DeleteSession") {
      Input input;
      input.set_type(Input::DELETE_SESSION);
      input.set_id(it->second);
      Output output;
      Call(input, &output, &error);
      sessions_.erase(it);
      return Response(event_id, session_id, output);
    }

    Input input;
    if (command == "SendKey") {
      input.set_type(Input::SEND_KEY);
      if (!ParseKeys(items, 3, input.mutable_key(), &error)) {
        return ErrorResponse(event_id, "wrong-format", error);
      }
    } else if (command == "SendCommand") {
      input.set_type(Input::SEND_COMMAND);
      if (items.size() < 4 ||
          !FillMessage(items[3], input.mutable_command(), &error)) {
        return ErrorResponse(event_id, "wrong-format",
                             error.empty() ? "ALIST is expected" : error);
      }
    } else if (command == "SendInput") {
      if (items.size() < 4 || !FillMessage(items[3], &input, &error)) {
        return ErrorResponse(event_id, "wrong-format",
                             error.empty() ? "ALIST is expected" : error);
      }
    } else {
      return ErrorResponse(event_id, "wrong-format",
                           "unknown command: " + command);
    }

    Output output;
    if (!CallWithSession(session_id, &input, &output, &error)) {
      return ErrorResponse(event_id, "session-failure", error);
    }
    return Response(event_id, session_id, output);
  }

 private:
  bool CreateSession(uint64_t *server_id, std::string *error) {
    Input input;
    input.set_type(Input::CREATE_SESSION);
    input.mutable_capability()->set_text_deletion(
        mozc::commands::Capability::DELETE_PRECEDING_TEXT);
    input.mutable_application_info()->set_process_id(getpid());
    Output output;
    if (!Call(input, &output, error)) {
      return false;
    }
    if (output.error_code() != Output::SESSION_SUCCESS || !output.has_id()) {
      *error = "mozc_server refused to create a session";
      return false;
    }
    *server_id = output.id();
    last_output_ = output;
    return true;
  }

  /*
   * Sends INPUT for SESSION_ID. If mozc_server has forgotten the
   * session (for example it was restarted or the session timed out),
   * a new session is created and INPUT is sent again once.
   */
  bool CallWithSession(uint32_t session_id, Input *input, Output *output,
                       std::string *error) {
    input->set_id(sessions_[session_id]);
    if (!Call(*input, output, error)) {
      return false;
    }
    if (output->error_code() == Output::SESSION_SUCCESS) {
      return true;
    }
    uint64_t server_id;
    if (!CreateSession(&server_id, error)) {
      return false;
    }
    sessions_[session_id] = server_id;
    input->set_id(server_id);
    return Call(*input, output, error);
  }

  /*
   * mozc_server is launched asynchronously by mozc_emacs_helper and
   * takes a while to load its dictionary and to publish
   * .session.ipc. Polls until the server answers a NO_OPERATION.
   */
  bool WaitForServer(std::string *error) {
    const int kMaxTrials = kServerWaitSeconds * 10;
    for (int i = 0; i < kMaxTrials; i++) {
      if (ipc_.LoadPathInfo(error)) {
        Input input;
        input.set_type(Input::NO_OPERATION);
        Output output;
        if (ipc_.Call(input, &output, error)) {
          return true;
        }
      }
      usleep(100 * 1000);
    }
    return false;
  }

  /*
   * Calls mozc_server. On a connection failure, asks the keeper to
   * (re)launch mozc_server, waits for it (the IPC key changes when
   * the server restarts) and retries once.
   */
  bool Call(const Input &input, Output *output, std::string *error) {
    if (ipc_.loaded() && ipc_.Call(input, output, error)) {
      return true;
    }
    std::string first_error = *error;
    if (!keeper_.EnsureServer()) {
      if (keeper_.enabled()) {
        *error = first_error + " (and mozc_emacs_helper couldn't start "
                               "mozc_server)";
        return false;
      }
    }
    if (!WaitForServer(error)) {
      return false;
    }
    return ipc_.Call(input, output, error);
  }

  /*
   * Keys are integers (key_code), strings (key_string), modifier
   * symbols, special key symbols or a KeyEvent alist.
   */
  bool ParseKeys(const std::vector<ValuePtr> &items, size_t start,
                 KeyEvent *key, std::string *error) {
    for (size_t i = start; i < items.size(); i++) {
      const ValuePtr &item = items[i];
      if (!item) {
        continue;
      }
      switch (item->kind) {
        case Value::INT:
          key->set_key_code((uint32_t)item->i);
          break;
        case Value::STR:
          key->set_key_string(item->s);
          break;
        case Value::SYM: {
          std::string name = NormalizeSymbol(item->s);
          if (name == "shift") {
            key->add_modifier_keys(KeyEvent::SHIFT);
          } else if (name == "ctrl" || name == "control") {
            key->add_modifier_keys(KeyEvent::CTRL);
          } else if (name == "alt" || name == "meta") {
            key->add_modifier_keys(KeyEvent::ALT);
          } else if (name == "caps") {
            key->add_modifier_keys(KeyEvent::CAPS);
          } else {
            const pb::EnumValueDescriptor *value =
                KeyEvent::SpecialKey_descriptor()->FindValueByName(
                    DenormalizeSymbol(name));
            if (!value) {
              *error = "unknown key: " + item->s;
              return false;
            }
            key->set_special_key(
                static_cast<KeyEvent::SpecialKey>(value->number()));
          }
          break;
        }
        case Value::CONS:
          if (!FillMessage(item, key, error)) {
            return false;
          }
          break;
        default:
          break;
      }
    }
    return true;
  }

  static std::string Response(int64_t event_id, uint32_t session_id,
                              const Output &output) {
    Output copy = output;
    /* Like mozc_emacs_helper: usage data is big and unused. */
    if (copy.has_candidate_window() && copy.candidate_window().has_usages()) {
      copy.mutable_candidate_window()->clear_usages();
    }
    /*
     * Drop the fields mozc.scm never reads. all_candidate_words holds
     * every candidate (can be thousands), so serializing, piping and
     * re-parsing it on every keystroke dominates the cost of moving the
     * selection. The page shown to the user comes from candidate_window.
     */
    copy.clear_all_candidate_words();
    copy.clear_removed_candidate_words_for_debug();
    copy.clear_incognito_candidate_words();
    std::string response = "((event-id . ";
    response += std::to_string(event_id);
    response += ")(session-id . ";
    response += std::to_string(session_id);
    response += ")(output . ";
    PrintMessage(copy, &response);
    response += "))";
    return response;
  }

  static std::string ErrorResponse(int64_t event_id, const std::string &error,
                                   const std::string &message) {
    std::string response = "((event-id . ";
    response += std::to_string(event_id);
    response += ")(error . " + error + ")";
    response += "(message . " + QuoteString(message) + "))";
    return response;
  }

  ServerKeeper keeper_;
  IPCClient ipc_;
  std::map<uint32_t, uint64_t> sessions_;
  uint32_t next_session_id_;
  Output last_output_;
  bool initialized_;
};

void Usage(FILE *out) {
  fprintf(out,
          "Usage: %s [OPTIONS]\n"
          "\n"
          "Options:\n"
          "  --emacs-helper=PATH  mozc_emacs_helper command used to\n"
          "                       launch mozc_server\n"
          "                       (default: $UIM_MOZC_EMACS_HELPER or\n"
          "                       mozc_emacs_helper)\n"
          "  --no-emacs-helper    Don't use mozc_emacs_helper\n"
          "  --help               Show this help\n"
          "  --version            Show version\n",
          kProgramName);
}

}  // namespace

int main(int argc, char **argv) {
  GOOGLE_PROTOBUF_VERIFY_VERSION;
  signal(SIGPIPE, SIG_IGN);

  std::string emacs_helper = "mozc_emacs_helper";
  const char *env = getenv("UIM_MOZC_EMACS_HELPER");
  if (env && *env) {
    emacs_helper = env;
  }
  for (int i = 1; i < argc; i++) {
    std::string arg = argv[i];
    if (arg == "--help" || arg == "-h") {
      Usage(stdout);
      return 0;
    } else if (arg == "--version") {
#ifdef UIM_VERSION
      printf("%s %s\n", kProgramName, UIM_VERSION);
#else
      printf("%s\n", kProgramName);
#endif
      return 0;
    } else if (arg == "--no-emacs-helper") {
      emacs_helper = "";
    } else if (arg.compare(0, 15, "--emacs-helper=") == 0) {
      emacs_helper = arg.substr(15);
    } else {
      Usage(stderr);
      return 1;
    }
  }

  /*
   * Stay silent on stdout until the first request. uim talks to us
   * via process-io, which reads our early stdout to detect exec
   * failures; an unsolicited greeting would be misread. uim sends
   * "(EVENT_ID Hello)" first and we answer with the greeting.
   */
  Bridge bridge(emacs_helper);

  std::string line;
  int c;
  while ((c = fgetc(stdin)) != EOF) {
    if (c != '\n') {
      line += static_cast<char>(c);
      continue;
    }
    if (line.empty()) {
      continue;
    }
    std::string response = bridge.Process(line);
    printf("%s\n", response.c_str());
    fflush(stdout);
    line.clear();
  }
  /* Handle a last request that wasn't newline-terminated before EOF. */
  if (!line.empty()) {
    std::string response = bridge.Process(line);
    printf("%s\n", response.c_str());
    fflush(stdout);
  }

  google::protobuf::ShutdownProtobufLibrary();
  return 0;
}
