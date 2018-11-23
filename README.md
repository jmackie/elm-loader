# elm-loader

Yet another [Webpack][webpack-home] loader for the [Elm][elm-home] programming language.

It's written with the following goals in mind:

-   Work flawlessly with **Webpack v4** and **Elm 0.19**, don't try and be backwards compatible.
-   Work robustly and provide useful errors - hence it's actually written in [Purescript][purescript-home]. This is more important to me than performance.
-   Zero assumptions. Anything uncertain is passed in via `options`. For example, is the loader running in "watch mode"? I dunno, you tell me.
-   Zero `npm` dependencies.

# Installation

TODO

-   from github
-   don't fancy the name war on npm

# How to use it

`webpack.config.js`

```javascript
module.exports = {
    ...

    module: {
        rules: [
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                use: [
                    {
                        loader: 'elm-loader',
                        options: {
                            debug: true,
                            watch: true,
                            verbose: true
                        },
                    },
                ],
            },
        ],
    },

};
```

# Options

#### `compiler`: string

#### `debug`: boolean

#### `optimize`: boolean

#### `watch`: boolean

#### `onCompileBegin`: function()

#### `onCompileFinish`: function()

#### `cwd`: string

[elm-home]: https://elm-lang.org/
[purescript-home]: http://www.purescript.org/
[webpack-home]: https://webpack.js.org/
