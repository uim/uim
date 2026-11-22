# Action handling framework

The action handling framework, `scm/action.scm`, shows the states of
an input method such as its input mode in toolbars and lets a user
change them from there. An input method only describes what can be
operated. The framework sends them to toolbars and processes the
operations from toolbars.

This document is for developers of input methods.

## Concepts

### Action

An action is one operation that a user can choose, such as "switch to
the hiragana input mode". It is a menu item in a toolbar. An action has
the following:

| Member | Description |
| --- | --- |
| ID | A symbol that identifies the action such as `action_skk_hiragana`. |
| Indication handler | A procedure that returns the indication of the action. |
| Activity predicate | A procedure that returns whether the state that the action sets is the current state. The action is shown as selected while it is true. |
| Handler | A procedure that is called when a user chooses the action. It changes the state. |

All the procedures take the owner as their only argument. The owner
is the context of the input method that has the action.

### Indication

An indication is how an action is shown. It is a list of the
following:

| Member | Description |
| --- | --- |
| ID | A symbol that specifies the icon such as `ja_hiragana`. Toolbars show the icon of this name in `pixmaps/`. |
| Iconic label | A very short string, typically one character, that is shown when the icon isn't available such as `"あ"`. |
| Label | A short string that is shown in the menu such as `"Hiragana"`. |
| Short description | A description that is used as a tooltip such as `"Hiragana input mode"`. |

### Widget

A widget is a group of actions. It is a button with a menu in a
toolbar. For example, `widget_skk_input_mode` has the actions for the
hiragana, katakana, half-width katakana, direct and full-width
alphanumeric input modes. A widget has the following:

| Member | Description |
| --- | --- |
| ID | A symbol that identifies the widget such as `widget_skk_input_mode`. |
| Indicator | What the button itself shows. Usually it is the indication of the action whose activity predicate is true. |
| Actions | The actions in the menu. |

At most one action of a widget should be active at a time. The
framework treats the widget as having no active action if two or more
activity predicates are true. The button shows "unknown" while no
action is active.

## API

An input method must use only the following 7 procedures of
`scm/action.scm`, and `context-list-replace-widgets!` for
customization. Don't write `(require "action.scm")` in an input
method. `scm/im.scm` prepares them.

### `(register-action id indication-handler activity-pred handler)`

Registers an action. `activity-pred` can be `#f` for an action that
has no state, and `handler` can be `#f` for an action that does
nothing.

### `(actions-new action-ids)`

Returns the actions of `action-ids`, a list of the IDs of the
registered actions. Unregistered ones are ignored.

### `(activity-indicator-new action-ids)`

Returns an indicator that shows the indication of the active one of
the actions of `action-ids`. This is what an ordinary widget uses.

### `(indicator-new indication-handler)`

Returns an indicator that shows what `indication-handler` returns.
Use this for a widget whose button doesn't depend on its actions.

### `(register-widget id indicator actions)`

Registers a widget. `actions` can be `#f` for a widget that has no
menu.

If the variable `default-ID` is defined, where `ID` is the ID of the
widget, and its value is the ID of one of the actions, the handler of
the action is called when the widget is created for a context. For
example, `default-widget_skk_input_mode` is the default input mode of
SKK.

### `context-prop-activate-handler`

Pass this to `register-im` as the handler of the property
activation. It calls the handler of the action that a user chose in a
toolbar.

### `context-mode-handler`

Pass this to `register-im` as the mode handler. It is for the legacy
mode API: the mode N is the Nth action, from 0, of the widget whose ID
ends with `_input_mode`.

## Example

This is an example that runs by itself. It has a widget for the input
mode that has two actions, "Off" and "On":

```scheme
;; An input method doesn't need this: scm/im.scm prepares the framework.
(require "action.scm")

;; The owner is the context of an input method. Here, it is a list
;; whose first element is the current input mode.
(define (sample-context-new)
  (list 'off))
(define (sample-context-mode sc)
  (car sc))
(define (sample-context-set-mode! sc mode)
  (set-car! sc mode))

(register-action 'action_sample_off
                 (lambda (sc)
                   (list 'sample_off "-" "Off" "Sample off mode"))
                 (lambda (sc)
                   (eq? (sample-context-mode sc) 'off))
                 (lambda (sc)
                   (sample-context-set-mode! sc 'off)))

(register-action 'action_sample_on
                 (lambda (sc)
                   (list 'sample_on "S" "On" "Sample on mode"))
                 (lambda (sc)
                   (eq? (sample-context-mode sc) 'on))
                 (lambda (sc)
                   (sample-context-set-mode! sc 'on)))

(define sample-input-mode-actions '(action_sample_off action_sample_on))

(register-widget 'widget_sample_input_mode
                 (activity-indicator-new sample-input-mode-actions)
                 (actions-new sample-input-mode-actions))

;; The action that is chosen when the widget is created.
(define default-widget_sample_input_mode 'action_sample_on)

;; The following is what the framework does for an input method.

;; It creates the widget for a context.
(define sc (sample-context-new))
(define widget (widget-new 'widget_sample_input_mode sc))
(display "mode: ")
(write (sample-context-mode sc))
(newline)

;; It sends this to toolbars.
(display (widget-compose-live-branch widget))

;; It calls the handler of the action that a user chose in a toolbar.
(widget-activate! widget 'action_sample_off)
(display "mode: ")
(write (sample-context-mode sc))
(newline)
(display (widget-compose-live-branch widget))
```

Run it with `uim-sh`:

```console
$ uim-sh sample.scm
mode: on
branch	sample_on	S	On
leaf	sample_off	-	Off	Sample off mode	action_sample_off
leaf	sample_on	S	On	Sample on mode	action_sample_on	*
mode: off
branch	sample_off	-	Off
leaf	sample_off	-	Off	Sample off mode	action_sample_off	*
leaf	sample_on	S	On	Sample on mode	action_sample_on
```

The lines of `branch` and `leaf` are what toolbars receive. A `branch`
is the button and a `leaf` is a menu item. The fields are separated by
a tab. The last field of the `leaf` of the active action is `*`.

## How to use in an input method

An input method does the same as the example to register its actions
and widgets. The differences are the following. `scm/look.scm` is a
small input method that uses them.

1. Don't use `widget-new`, `widget-compose-live-branch` and
   `widget-activate!`. The framework does them.

2. Name the widget for the input mode `widget_NAME_input_mode`, where
   `NAME` is the name of the input method. Some toolbars show only
   this widget and the legacy mode API uses it.

3. Mark the label and the short description of an indication with `N_`
   for translation such as `(N_ "Sleep")`.

4. Set the IDs of the widgets to show, in the order to show, to the
   `widgets` member of the context when the context is created:

   ```scheme
   (define look-widgets '(widget_look_input_mode))

   (define (look-context-new . args)
     (let ((lc (apply look-context-new-internal args)))
       (look-context-set-widgets! lc look-widgets)
       lc))
   ```

5. Pass `context-mode-handler` and `context-prop-activate-handler` to
   `register-im`.

The input method doesn't need to notify toolbars of a change: after
each handler of the input method such as the key press handler
returns, the framework checks the widgets and notifies toolbars only
when what they show has been changed.

## Customization

The widgets to show, their actions and their default actions can be
customs. See `scm/skk-custom.scm` for an example: `skk-widgets`,
`skk-input-mode-actions` and `default-widget_skk_input_mode`.

Register the widgets again when one of them is changed. To apply the
change to the existing contexts too, call
`(context-list-replace-widgets! NAME widget-ids)`, where `NAME` is
the name of the input method, after it. This is a simplified one of
SKK:

```scheme
(define (skk-configure-widgets)
  (register-widget 'widget_skk_input_mode
                   (activity-indicator-new skk-input-mode-actions)
                   (actions-new skk-input-mode-actions))
  (context-list-replace-widgets! 'skk skk-widgets))

(custom-add-hook 'skk-input-mode-actions
                 'custom-set-hooks
                 (lambda ()
                   (skk-configure-widgets)))
```

## How it works

- `setup-context` of `scm/im.scm` creates the widgets of a new context
  from the IDs in its `widgets` member and sends them to toolbars.
- A widget is sent as a branch and leaves of the `prop_list_update`
  message as the example shows.
- When a user chooses a menu item, the toolbar sends the
  `prop_activate` message with the ID of the action.
  `context-prop-activate-handler` finds the action in the widgets of
  the context and calls its handler.
- A context that has no widget shows `widget_fallback`, which shows
  "unknown".

See `doc/HELPER-PROTOCOL` for the messages.

## Test

`test/test-action.scm` has the tests of the framework. They are also
examples of the procedures in this document.
