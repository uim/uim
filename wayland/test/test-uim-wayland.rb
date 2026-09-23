#!/usr/bin/env ruby
#
# Copyright (c) 2026 uim Project https://github.com/uim/uim
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. Neither the name of authors nor the names of its contributors
#    may be used to endorse or promote products derived from this software
#    without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
# IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
# THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
# OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# A compositor that implements just enough of wl_compositor, wl_shm,
# zwp_input_method_v1 and zwp_input_panel_v1 to stand in for KWin and
# Weston. It hands uim-wayland one end of a socketpair in
# WAYLAND_SOCKET, exactly as they do, activates a context, grabs the
# keyboard and sends a key sequence.
#
# No display and no compositor are needed, so this runs unattended.
#
# The wire format is read out of the protocol descriptions that
# wayland-scanner uses, so nothing here has to be kept in step with
# them by hand.
#
# Set UIM_WAYLAND to the input method and run test/run.rb, and put
# UIM_WAYLAND_DEBUG or WAYLAND_DEBUG in the input method's environment
# to see what it makes of all this.

require "rexml/document"
require "socket"
require "tempfile"
require "test/unit"

# The protocol descriptions, as wayland-scanner reads them.
module Wayland
  Argument = Struct.new(:name, :type, :interface)
  Message = Struct.new(:name, :opcode, :arguments, :destructor)
  Interface = Struct.new(:name, :requests, :events) do
    def request(opcode)
      requests[opcode]
    end

    def event(name)
      events.find {|event| event.name == name} or
        raise ArgumentError, "#{self.name} has no event #{name}"
    end
  end

  class Protocol
    def initialize(*paths)
      @interfaces = {}
      paths.each {|path| load(path)}
    end

    def [](name)
      @interfaces[name]
    end

    private
    def load(path)
      document = REXML::Document.new(File.read(path))
      document.elements.each("protocol/interface") do |element|
        name = element.attributes["name"]
        interface = Interface.new(name, [], [])
        @interfaces[name] = interface
        read_messages(element, "request", interface.requests)
        read_messages(element, "event", interface.events)
      end
    end

    def read_messages(element, kind, messages)
      element.elements.each(kind) do |element|
        arguments = []
        element.elements.each("arg") do |argument|
          arguments << Argument.new(argument.attributes["name"],
                                    argument.attributes["type"],
                                    argument.attributes["interface"])
        end
        messages << Message.new(element.attributes["name"],
                                messages.size,
                                arguments,
                                element.attributes["type"] == "destructor")
      end
    end
  end

  # One message is an object ID, a size and an opcode, then the
  # arguments. File descriptors travel beside the message, not in it.
  class Connection
    DISPLAY_ID = 1
    FIRST_SERVER_ID = 0xff000000

    attr_reader :protocol

    def initialize(socket, protocol)
      @socket = socket
      @protocol = protocol
      @objects = {DISPLAY_ID => "wl_display"}
      @buffer = +""
      @descriptors = []
      @next_server_id = FIRST_SERVER_ID
      @serial = 0
      @closed = false
    end

    def closed?
      @closed
    end

    def close
      return if @closed
      @closed = true
      @socket.close
    end

    def next_serial
      @serial += 1
    end

    def interface_of(id)
      @objects[id]
    end

    def register(id, interface)
      @objects[id] = interface
    end

    def create_object(interface)
      id = @next_server_id
      @next_server_id += 1
      register(id, interface)
      id
    end

    # Reads whatever has arrived and yields one decoded request at a
    # time as [id, interface, message, arguments].
    def each_request(timeout)
      return if @closed
      return unless IO.select([@socket], nil, nil, timeout)

      begin
        data, _, _, *controls = @socket.recvmsg(4096, 0, nil, scm_rights: true)
      rescue EOFError, Errno::ECONNRESET
        @closed = true
        return
      end
      if data.nil? or data.empty?
        @closed = true
        return
      end
      controls.each do |control|
        next unless control.cmsg_is?(:SOCKET, :RIGHTS)
        @descriptors.concat(control.unix_rights)
      end
      @buffer << data

      while (request = take_request)
        yield(*request)
      end
    end

    def send_event(id, event_name, *values, descriptor: nil)
      interface = @protocol[@objects.fetch(id)]
      event = interface.event(event_name)
      body = +""
      event.arguments.zip(values) do |argument, value|
        next if argument.type == "fd"
        body << encode(argument, value)
      end
      header = [id, ((body.bytesize + 8) << 16) | event.opcode].pack("VV")
      write(header + body, descriptor)
    end

    def delete_id(id)
      @objects.delete(id)
      send_event(DISPLAY_ID, "delete_id", id)
    end

    private
    def write(data, descriptor)
      if descriptor
        controls = [Socket::AncillaryData.unix_rights(descriptor)]
        @socket.sendmsg(data, 0, nil, *controls)
      else
        @socket.write(data)
      end
    end

    def take_request
      return nil if @buffer.bytesize < 8

      id, word = @buffer.unpack("VV")
      size = word >> 16
      return nil if @buffer.bytesize < size

      opcode = word & 0xffff
      body = @buffer.byteslice(8, size - 8)
      @buffer = @buffer.byteslice(size..) || +""

      interface_name = @objects[id]
      return [id, nil, nil, []] if interface_name.nil?

      interface = @protocol[interface_name]
      message = interface.request(opcode)
      [id, interface_name, message, decode(message, body)]
    end

    def decode(message, body)
      offset = 0
      message.arguments.collect do |argument|
        case argument.type
        when "int"
          value = body.unpack1("l<", offset: offset)
          offset += 4
        when "uint", "object"
          value = body.unpack1("V", offset: offset)
          offset += 4
        when "fixed"
          value = body.unpack1("l<", offset: offset) / 256.0
          offset += 4
        when "new_id"
          if argument.interface
            value = body.unpack1("V", offset: offset)
            offset += 4
            register(value, argument.interface)
          else
            # Only wl_registry.bind: the interface is named in the
            # message itself.
            length = body.unpack1("V", offset: offset)
            offset += 4
            name = body.byteslice(offset, length - 1)
            offset += padded(length)
            offset += 4 # version
            value = body.unpack1("V", offset: offset)
            offset += 4
            register(value, name)
          end
        when "string"
          length = body.unpack1("V", offset: offset)
          offset += 4
          if length.zero?
            value = nil
          else
            value = body.byteslice(offset, length - 1)
            value.force_encoding(Encoding::UTF_8)
          end
          offset += padded(length)
        when "array"
          length = body.unpack1("V", offset: offset)
          offset += 4
          value = body.byteslice(offset, length)
          offset += padded(length)
        when "fd"
          value = @descriptors.shift
        else
          raise "unknown argument type: #{argument.type}"
        end
        value
      end
    end

    def encode(argument, value)
      case argument.type
      when "int"
        [value].pack("l<")
      when "uint", "object", "new_id"
        [value].pack("V")
      when "fixed"
        [(value * 256).round].pack("l<")
      when "string"
        bytes = "#{value}\0".b
        [bytes.bytesize].pack("V") + bytes.ljust(padded(bytes.bytesize), "\0")
      when "array"
        [value.bytesize].pack("V") + value.ljust(padded(value.bytesize), "\0")
      else
        raise "cannot send argument type: #{argument.type}"
      end
    end

    def padded(size)
      (size + 3) & ~3
    end
  end
end

# The names the compositor and the test press keys by, and the states
# a key can be in.
module Keyboard
  # Linux evdev key codes, and the xkb ones are these plus 8.
  KEY_LEFT_CONTROL = 29
  KEY_A = 30
  KEY_J = 36
  KEY_K = 37

  RELEASED = 0
  PRESSED = 1
end

class Compositor
  include Keyboard

  # XKB numbers the real modifiers, so this doesn't depend on the keymap.
  MOD_CONTROL = 1 << 2

  # A keymap with only the keys the test presses. It is self contained,
  # so the machine running the test needs no keyboard descriptions of
  # its own.
  KEYMAP = <<~KEYMAP
    xkb_keymap {
      xkb_keycodes "uim-test" {
        minimum = 8;
        maximum = 255;
        <LCTL> = #{KEY_LEFT_CONTROL + 8};
        <AC01> = #{KEY_A + 8};
        <AC07> = #{KEY_J + 8};
        <AC08> = #{KEY_K + 8};
      };
      xkb_types "uim-test" {
        type "ONE_LEVEL" {
          modifiers = none;
          level_name[1] = "Any";
        };
        type "ALPHABETIC" {
          modifiers = Shift+Lock;
          map[Shift] = 2;
          map[Lock] = 2;
          level_name[1] = "Base";
          level_name[2] = "Caps";
        };
      };
      xkb_compatibility "uim-test" {
        interpret Control_L { action = SetMods(modifiers = Control); };
      };
      xkb_symbols "uim-test" {
        key <LCTL> { type = "ONE_LEVEL", [ Control_L ] };
        key <AC01> { type = "ALPHABETIC", [ a, A ] };
        key <AC07> { type = "ALPHABETIC", [ j, J ] };
        key <AC08> { type = "ALPHABETIC", [ k, K ] };
        modifier_map Control { Control_L };
      };
    };
  KEYMAP

  GLOBALS = {
    "wl_compositor" => 4,
    "wl_shm" => 1,
    "zwp_input_method_v1" => 1,
    "zwp_input_panel_v1" => 1,
  }

  # How long to wait for uim-wayland to answer. It is generous because
  # it is only reached when something is wrong.
  TIMEOUT = 15

  attr_reader :forwarded_keys, :commits, :preedits, :overlay_panels

  def initialize
    @connection = nil
    @process_id = nil
    @scm_file = nil
    @keymap = nil
    @input_method = nil
    @context = nil
    @keyboard = nil
    @time = 1000

    @forwarded_keys = []
    @commits = []
    @preedits = []
    @overlay_panels = 0
  end

  # Runs uim-wayland and waits until it has taken the keyboard, which
  # is the point from which it can be typed into.
  def start
    program = ENV["UIM_WAYLAND"]
    raise("UIM_WAYLAND is not set") if program.nil?

    paths = protocol_paths
    missing = paths.find {|path| path.nil? or not File.exist?(path)}
    raise("#{missing.inspect} is missing") if missing

    @scm_file = user_scm_file
    ours, theirs = UNIXSocket.socketpair(Socket::SOCK_STREAM)
    environment = {
      "WAYLAND_SOCKET" => "3",
      "WAYLAND_DISPLAY" => nil,
      "LIBUIM_USER_SCM_FILE" => @scm_file.path,
    }
    @process_id = Process.spawn(environment, program, 3 => theirs)
    theirs.close
    @connection = Wayland::Connection.new(ours,
                                          Wayland::Protocol.new(*paths))

    wait_for {@keyboard}
    raise("uim-wayland didn't take the keyboard") if @keyboard.nil?
    send_modifiers(0)
  end

  def stop
    if @connection and not @connection.closed?
      deactivate
      @connection.close
    end
    if @process_id
      begin
        Process.kill(:TERM, @process_id)
      rescue Errno::ESRCH
      end
      Process.waitpid(@process_id)
    end
    @keymap&.close!
    @scm_file&.close!
  end

  def activated?
    not @input_method.nil?
  end

  # Reads whatever uim-wayland has to say until the block is happy or
  # the time is up.
  def wait_for
    deadline = Time.now + TIMEOUT
    loop do
      break if yield
      break if @connection.closed?
      break if Time.now > deadline
      @connection.each_request(0.05) do |id, interface, message, arguments|
        handle(id, interface, message, arguments) unless message.nil?
      end
    end
  end

  def type(key)
    press(key)
    release(key)
  end

  # SKK leaves the keyboard alone until Ctrl-j puts it into hiragana.
  def switch_to_hiragana
    send_modifiers(MOD_CONTROL)
    press(KEY_LEFT_CONTROL)
    type(KEY_J)
    release(KEY_LEFT_CONTROL)
    send_modifiers(0)
  end

  private
  def protocol_paths
    core = ENV["WAYLAND_XML"] ||
           File.join(`pkg-config --variable=pkgdatadir wayland-scanner`.strip,
                     "wayland.xml")
    [core, ENV["INPUT_METHOD_XML"]]
  end

  # uim reads the default input method from this file, and it has to
  # be SKK for the keys the test presses to mean anything.
  def user_scm_file
    file = Tempfile.new("uim-wayland-test")
    file.puts("(define default-im-name 'skk)")
    file.flush
    file
  end

  def handle(id, interface, message, arguments)
    case "#{interface}.#{message.name}"
    when "wl_display.sync"
      callback = arguments[0]
      @connection.send_event(callback, "done", @connection.next_serial)
      @connection.delete_id(callback)
    when "wl_display.get_registry"
      registry = arguments[0]
      GLOBALS.each_with_index do |(global, version), index|
        @connection.send_event(registry, "global", index + 1, global, version)
      end
    when "wl_registry.bind"
      bound_id = arguments.last
      if @connection.interface_of(bound_id) == "zwp_input_method_v1"
        @input_method = bound_id
        activate
      end
    when "wl_shm.create_pool"
      arguments[1]&.close
    when "wl_surface.attach"
      buffer = arguments[0]
      # A compositor releases the buffer once it has read it, and the
      # candidate window waits for that before it draws again.
      @connection.send_event(buffer, "release") unless buffer.zero?
    when "zwp_input_panel_surface_v1.set_overlay_panel"
      @overlay_panels += 1
    when "zwp_input_method_context_v1.grab_keyboard"
      @keyboard = arguments[0]
      send_keymap
    when "zwp_input_method_context_v1.commit_string"
      @commits << arguments[1]
    when "zwp_input_method_context_v1.preedit_string"
      @preedits << arguments[1]
    when "zwp_input_method_context_v1.key"
      @forwarded_keys << [arguments[2], arguments[3]]
    end

    @connection.delete_id(id) if message.destructor
  end

  def activate
    @context = @connection.create_object("zwp_input_method_context_v1")
    @connection.send_event(@input_method, "activate", @context)
    @connection.send_event(@context, "commit_state", 1)
  end

  def deactivate
    return if @input_method.nil? or @context.nil?
    @connection.send_event(@input_method, "deactivate", @context)
  end

  def send_keymap
    @keymap = Tempfile.new("uim-wayland-test-keymap")
    @keymap.write(KEYMAP)
    @keymap.flush
    @connection.send_event(@keyboard, "keymap", 1, nil, KEYMAP.bytesize,
                           descriptor: @keymap.to_io)
  end

  def press(key)
    send_key(key, PRESSED)
  end

  def release(key)
    send_key(key, RELEASED)
  end

  def send_key(key, state)
    @time += 1
    @connection.send_event(@keyboard, "key", @connection.next_serial, @time,
                           key, state)
  end

  def send_modifiers(depressed)
    @connection.send_event(@keyboard, "modifiers", @connection.next_serial,
                           depressed, 0, 0, 0)
  end
end

class UimWaylandTest < Test::Unit::TestCase
  include Keyboard

  # Hiragana ka. It is an escape because power_assert re-reads this
  # file to build its message, and a machine without a UTF-8 locale
  # can't read a source that isn't ASCII.
  KA = "\u304B"

  def setup
    @compositor = Compositor.new
    begin
      @compositor.start
      yield
    ensure
      @compositor.stop
    end
  end

  def test_input_method_activated
    assert do
      @compositor.activated?
    end
  end

  def test_unconsumed_key_is_forwarded
    # The input method is off, so uim doesn't consume this and the
    # compositor has to get it back to pass on to the application.
    @compositor.type(KEY_A)
    forwarded_keys = [[KEY_A, PRESSED], [KEY_A, RELEASED]]
    @compositor.wait_for {@compositor.forwarded_keys == forwarded_keys}
    assert_equal(forwarded_keys, @compositor.forwarded_keys)
  end

  def test_preedit
    @compositor.switch_to_hiragana
    @compositor.type(KEY_K)
    @compositor.wait_for {@compositor.preedits == ["k"]}
    assert_equal(["k"], @compositor.preedits)
  end

  def test_commit
    @compositor.switch_to_hiragana
    @compositor.type(KEY_K)
    @compositor.type(KEY_A)
    @compositor.wait_for {@compositor.commits == [KA]}
    assert_equal([KA], @compositor.commits)
  end

  def test_candidate_window_is_an_overlay_panel
    @compositor.wait_for {@compositor.overlay_panels == 1}
    assert_equal(1, @compositor.overlay_panels)
  end
end
