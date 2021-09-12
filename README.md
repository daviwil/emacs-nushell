# emacs-nushell

Experiments in integrating Emacs and Nushell!

Here's the command I was running:

```
ls | to json | hash base64 | $"(char lparen)nu/display-output (char dquote)($in)(char dquote)(char rparen)" | emacsclient -e $in
```

### Streams

Check out the stream recordings here:

1. [Integrating Nushell and Emacs](https://www.youtube.com/watch?v=IHeKUeO7bpo)
