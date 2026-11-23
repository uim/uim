# Custom

This document describes usage of the uim-custom API and facility for
developers of input methods.

## Overview

A custom is a variable of an input method that a user can change with
preference tools such as uim-pref and `uim-custom-cli`. It has a type,
a label and a description in addition to its value, so that preference
tools can show a suitable editor for it and validate a new value.

The value that a user saved is stored into
`~/.uim.d/customs/custom-GROUP.scm`, where `GROUP` is the primary
group of the custom. It is loaded when the file that defines the
custom is loaded with `require-custom`. It isn't loaded when
`LIBUIM_VANILLA` is set. A change by a preference tool is also sent to
the running processes through uim-helper-server.

There are two implementations of the custom API:

- `scm/custom-rt.scm` is the lightweight one for input. It only
  defines the variables and loads the saved values. It is used by
  default.
- `scm/custom.scm` is the full-featured one for preference tools. It
  also keeps the types, the labels and the descriptions and validates
  values.

A file that defines customs works with both of them.

## How to define your own customs

### `(define-custom-group group label description)`

Defines a group of customs. `group` is a symbol. `label` and
`description` are strings for preference tools.

### `(define-custom name default groups type label description)`

Defines a custom. It defines the variable `name` with `default` if the
variable isn't defined.

- `name`: A symbol.
- `default`: The default value.
- `groups`: A list of the symbols of the groups that the custom
  belongs to. All of them must be defined with `define-custom-group`.
  The first one is the primary group. It decides the file that the
  value is saved into. The others are subgroups. Preference tools use
  them as sections in the page of the primary group. The subgroup is
  `main` when only the primary group is specified.
- `type`: A list of the name of the type and its attributes. See the
  following table.
- `label` and `description`: Strings for preference tools.

| Type | Value | Attributes |
| --- | --- | --- |
| `(boolean)` | `#t` or `#f`. | None. |
| `(integer MIN MAX)` | An integer between `MIN` and `MAX`. | The range. |
| `(string REGEX)` | A string. | `REGEX` isn't used for validation now. Use `".*"`. |
| `(pathname TYPE)` | A string of a path. | `TYPE` is `regular-file` or `directory`. |
| `(choice ITEM ...)` | One of the symbols of the items. | Each item is a list of a symbol, a label and a description. |
| `(ordered-list ITEM ...)` | A list of the symbols of the items. | The same as `choice`. |
| `(key)` | A list of keys. A key is a string such as `"<Control>j"` or the symbol of another custom of this type. | None. |
| `(table COLUMN ...)` | A list of rows. A row is a list of strings. | Each column is a list of a symbol, a label and a description. |

A custom of the `key` type also defines the predicate `name?` such as
`sample-on-key?` for `sample-on-key`. It takes the key and the state
of a key event and returns whether the event is one of the keys.

Mark the labels and the descriptions with `N_` for translation in an
input method such as `(N_ "Show annotation")`.

### Example

This is an example that runs by itself:

```scheme
;; An input method doesn't need this: this is for the full-featured
;; custom API such as the validation in this example.
(require "custom.scm")

(define-custom-group 'sample
                     "Sample"
                     "A sample input method")

(define-custom 'sample-show-annotation? #t
  '(sample)
  '(boolean)
  "Show annotation"
  "Show the annotation of a candidate in the candidate window.")

(define-custom 'sample-nr-candidate-max 10
  '(sample)
  '(integer 1 20)
  "Number of candidates"
  "The number of candidates in the candidate window at a time.")

(define-custom 'sample-server-name "localhost"
  '(sample)
  '(string ".*")
  "Server name"
  "The host name of the conversion server.")

(define-custom 'sample-dictionary "/usr/share/sample/dictionary"
  '(sample)
  '(pathname regular-file)
  "Dictionary"
  "The path of the dictionary.")

(define-custom 'sample-input-style 'romaji
  '(sample)
  (list 'choice
        (list 'romaji "Romaji" "Romaji input")
        (list 'kana "Kana" "Kana input"))
  "Input style"
  "How to input kana.")

(define-custom 'sample-buttons '(mode style)
  '(sample)
  (list 'ordered-list
        (list 'mode "Mode" "Input mode")
        (list 'style "Style" "Input style")
        (list 'tool "Tool" "Dictionary tool"))
  "Toolbar buttons"
  "The buttons to show in the order.")

(define-custom 'sample-on-key '("<Control>j" "zenkaku-hankaku")
  '(sample)
  '(key)
  "On"
  "The keys to turn on the input method.")

;; A custom is a variable.
(display "sample-nr-candidate-max: ")
(write sample-nr-candidate-max)
(newline)

;; A new value is validated with the type.
(display "set 5: ")
(write (custom-set-value! 'sample-nr-candidate-max 5))
(display ", sample-nr-candidate-max: ")
(write sample-nr-candidate-max)
(newline)
(display "set 100: ")
(write (custom-set-value! 'sample-nr-candidate-max 100))
(display ", sample-nr-candidate-max: ")
(write sample-nr-candidate-max)
(newline)

(display "set kana: ")
(write (custom-set-value! 'sample-input-style 'kana))
(display ", set unknown: ")
(write (custom-set-value! 'sample-input-style 'unknown))
(display ", sample-input-style: ")
(write sample-input-style)
(newline)

;; A custom of the key type also defines a predicate for a key event.
;; 106 is the key code of "j".
(display "sample-on-key? with Control+j: ")
(write (sample-on-key? 106 (cdr (assq 'Control_key key-state-alist))))
(display ", with j: ")
(write (sample-on-key? 106 0))
(newline)
```

Run it with `uim-sh`:

```console
$ uim-sh sample-custom.scm
sample-nr-candidate-max: 10
set 5: #t, sample-nr-candidate-max: 5
set 100: #f, sample-nr-candidate-max: 5
set kana: #t, set unknown: #f, sample-input-style: kana
sample-on-key? with Control+j: #t, with j: #f
```

## How to use customs in an input method

Write the definitions of the customs of an input method into a file
such as `sample-custom.scm` and load it in the file of the input
method with `require-custom`:

```scheme
(require-custom "sample-custom.scm")
```

Don't use `require` to load it. `require-custom` also loads the values
that a user saved and remembers the file to reload it later. `require`
doesn't.

The suffix `-custom` of the file name isn't required. But it is the
convention of uim.

The input method can refer to the customs as variables after it.

## Run a procedure when a custom is changed

Add a procedure that takes no argument to `custom-set-hooks` of the
custom with `custom-add-hook`. It is called whenever the custom is
set. A change by a preference tool also calls it in the running
processes. For example, an input method can reload its dictionary when
a user changes the path of the dictionary with uim-pref:

```scheme
(define-custom-group 'sample
                     "Sample"
                     "A sample input method")

(define-custom 'sample-dictionary "/usr/share/sample/dictionary"
  '(sample)
  '(pathname regular-file)
  "Dictionary"
  "The path of the dictionary.")

(define (sample-reload-dictionary)
  (display "reload: ")
  (display sample-dictionary)
  (newline))

(custom-add-hook 'sample-dictionary
                 'custom-set-hooks
                 sample-reload-dictionary)

(custom-set-value! 'sample-dictionary "/tmp/dictionary")
```

```console
$ uim-sh sample-set-hook.scm
reload: /tmp/dictionary
```

There are limitations:

- `scm/custom-rt.scm` accepts only one procedure for a custom. The last
  one is used. `scm/custom.scm` accepts two or more.

- A procedure that is only in `scm/custom.scm` can't be used in the
  procedure. Add such a procedure only with `scm/custom.scm`:

  ```scheme
  (if custom-full-featured?
      (custom-add-hook 'anthy-kana-input-method-actions
                       'custom-set-hooks
                       (lambda ()
                         (custom-choice-range-reflect-olist-val
                          'default-widget_anthy_kana_input_method
                          'anthy-kana-input-method-actions
                          anthy-kana-input-method-indication-alist))))
  ```

## Control whether a custom is active

A custom has a state named activity. It shows whether the value of the
custom makes sense now. A preference tool disables the editor of an
inactive custom. For example, the name of a server makes sense only
while the server is used.

Add a predicate that takes no argument to `custom-activity-hooks` of
the custom with `custom-add-hook`. A custom that has no predicate is
always active.

```scheme
;; The activity is only in the full-featured custom API. An input
;; method doesn't need this.
(require "custom.scm")

(define-custom-group 'sample
                     "Sample"
                     "A sample input method")

(define-custom 'sample-use-server? #f
  '(sample)
  '(boolean)
  "Use a server"
  "Use a conversion server.")

(define-custom 'sample-server-name "localhost"
  '(sample)
  '(string ".*")
  "Server name"
  "The host name of the conversion server.")

;; sample-server-name makes sense only while sample-use-server? is true.
(custom-add-hook 'sample-server-name
                 'custom-activity-hooks
                 (lambda ()
                   sample-use-server?))

(display "active: ")
(write (custom-active? 'sample-server-name))
(newline)

(custom-set-value! 'sample-use-server? #t)
(display "active: ")
(write (custom-active? 'sample-server-name))
(newline)
```

```console
$ uim-sh sample-activity.scm
active: #f
active: #t
```

The predicate can be any procedure. It doesn't need to refer to a
custom of the same group.

The predicates are evaluated when:

- A preference tool gets the custom with `uim_custom_get()`.
- `custom-active?` is called.
- Any custom is set. The predicates of all the customs are evaluated
  then, even when the value isn't changed, and preference tools are
  notified of the customs whose activity was changed through their
  callbacks.

`scm/custom-rt.scm` ignores `custom-activity-hooks`: the activity is
only for preference tools.
