```lang=caml
type 'a tree =
    | Leaf
    | Node of 'a * 'a tree * 'a tree

let rec count = function
    | Leaf                     -> 0
    | Node (node, left, right) -> 1 + count left + count right
```

```
dario@localhost:~/lambtex$ make
lambcmd -f lambtex -t xhtml -i sample.lambtex -o index.html

dario@localhost:~/lambtex$ file index.html
index.html: HTML document text
```

