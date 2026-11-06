# Literate KrabbaScript

Literate KrabbaScript is for when your `.ks` files have more comments than code, and you want to avoid the ugly `#` on every line.
The main audience is people writing libraries, maintaining/working with large codebases,
and other applications where you need a lot of comments to explain your code.

For example, instead of writing:

```
# receives lines without semicolons;
# returns lines with semicolons
# for example, this turns
# var foo: I32 = 0
# into
# var foo: I32 = 0;
func insertSemicolons(lines Arr<String>) Arr<String> {
}
```

You would write:

```
receives lines without semicolons;
returns lines with semicolons
for example, this turns
var foo: I32 = 0
into
var foo: I32 = 0;
    func insertSemicolons(lines Arr<String>) Arr<String> {
    }
```

Not only that, but you also get a static site generator for your documentation needs.

# Commands

Right now, there are no commands — only flags. If you do not use any flags then it will default to transpiling.

## Transpile

Here's an example command:

```
literate_krabbascript formatter.suomi formatter.ks
```

If you are doing something like that, then know that you can omit the output file argument and nothing would change:

```
literate_krabbascript formatter.suomi
```

These two commands are exactly the same.

### Removing prose

By default, all prose — any non indented line — will be transformed into a comment.

If you wish to filter out all prose, then pass in: "without prose".

```
literate_krabbascript formatter.suomi without prose
```

Note that this only refers to prose; the compiler won't hunt down every comment in your code and remove it.

## Generate

To generate a `.html` page, pass in the "make page" flag:

```
literate_krabbascript formatter.suomi make page
```

Like with transpiling, you can omit the output file and it will be inferred as the input file but with a `.html` extension;
the previous example is the same as:

```
literate_krabbascript formatter.suomi formatter.html make page
```

# 

You can use any extension for your literate files, but `.suomi` or `.litkrab` is preferred.

The compiler won't care about your extension, unless you omit an output file and have a .ks extension on your input file;
the extension cannot be inferred and I don't want to overwrite your literate code.

Literate KrabbaScript is heavily inspired by [Literate CoffeeScript](https://coffeescript.org/#literate).

[See the original KrabbaScript](https://github.com/KrabbaTek/krabbascript)