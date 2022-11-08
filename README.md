# programming-language-foundations
Programming Language Foundations (Stump 2013)

#### Run with:
`stack build && stack exec pfl-exe`

#### Example output:
```
5.8.1-1a. Syntax tree of: (λx.(λy.x y)) =
    λ
   / \
  x   λ
     / \
    y   @
       / \
      x   y

```