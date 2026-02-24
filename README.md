# sabela

Sabela is a reactive notebook environment for Haskell. The name is derived from the [Ndebele](https://en.wikipedia.org/wiki/Northern_Ndebele_language) word meaning "to respond." The project has two purposes. Firstly, it is an attempt to design and create a modern Haskell notebook where reactivity is a first class concern. Secondly, it is an experiment ground for package/environment management in Haskell notebooks (a significant pain point in IHaskell).

![A screenshot of the web ui](./static/images/screenshot.png)

## Quick start

```bash
git clone https://github.com/DataHaskell/sabela
cd sabela
cabal run
```

Open `localhost:3000/index.html` and explore `./examples/analysis.md` for a quick tutorial.

The execution and dependency management model is based on [scripths](https://github.com/DataHaskell/scripths).
