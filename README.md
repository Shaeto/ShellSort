# Shell's sort on Haskell

My first Haskell application written during studying of "Learn Haskell for Good" book :)

it is a simple implementation of Shell's sort method

```
cabal run
```

## How it works

```haskell
shellSort :: Ord a => [Int] -> [a] -> [a]
```

the **shellSort** function gets a list of gaps and a list of elements to be sorted

Based on the sequence of gaps, a **list of indices of all sublists** is formed

example of a generated list of sublist indices for 20 elements and standard gaps:

```
cabal repl
```

```
ghci> gapsToSublists 20 $ gapsShell 20
[[0,10],[1,11],[2,12],[3,13],[4,14],[5,15],[6,16],[7,17],[8,18],[9,19],[0,5,10,15],[1,6,11,16],[2,7,12,17],[3,8,13,18],[4,9,14,19],[0,2,4,6,8,10,12,14,16,18],[1,3,5,7,9,11,13,15,17,19],[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]]
```

Next, the sublists are sorted using **the insertion method** and we get the output elements sorted using the Shell method
