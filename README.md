# keyadvice.el

This package provides a functionality similar to "advice" package (`defadvice`) but for key bindings. You can advice any key binding to run your own code before or after the original action. This is useful if you want to run some conditional code together with the original (whatever that might be) binding. The original binding is a binding this key would normally invoke.

Inside the forms you provide to the macro you can use a special value "keyadvice-do-it" that will be replaced with code that will call the original function.

# Interface

The advices will only be enabled if `keyadvice-mode` is enabled in current buffer. You can also use `keyadvice-global-mode` to enable `keyadvice-mode` globally in all buffers (recommended).

To add a key binding advice use:

```scheme
(keyadvice-add-advice binding &rest forms)
```

*(removing/temporary disabling etc. will come later/on demand :)*

# Examples

If you use AUCTeX (if you don't you should) there's a special minor mode called `LaTeX-math-mode` that provides many special key bindings for inserting mathematics. The most "famous" key binding is "\`" command which takes additional letter(s) and expand them into LaTeX macros. For example "\` a" inserts `\alpha` in the buffer.

However, entering the `LaTeX-math-mode` manually each time is annoying. You can advice the \` binding to automatically check if the point is inside math enviroment and then execute the apropriate expansion. Otherwise, it should call the function that would be normally bound to \`.

```scheme
(keyadvice-add-advice (kbd "`")
  (if (and (eq major-mode 'latex-mode) (texmathp))
      (let* ((events (let ((overriding-local-map LaTeX-math-keymap))
                       (read-key-sequence "math: ")))
             (binding (lookup-key LaTeX-math-keymap events)))
        (call-interactively binding))
    keyadvice-do-it)) ;; special value expands to the original function call
```
