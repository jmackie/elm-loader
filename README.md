# `elm-loader`

![GitHub tag (latest SemVer)](https://img.shields.io/github/tag/jmackie/elm-loader.svg)

Yet another [Webpack][webpack-home] loader for the [Elm][elm-home] programming language.

It's written with the following goals in mind:

-   Work primarily with **Webpack v4** and **Elm 0.19**, don't try and be backwards compatible.
-   Work robustly and provide useful errors - hence it's actually written in [Purescript][purescript-home]. This is more important to me than performance.
-   Zero assumptions. Anything uncertain is passed in via `options`.
-   Zero `npm` dependencies.

# Installation

Just about every combination of `elm`, `webpack`, and `loader` is taken on `npm`. So this will live on Github for the forseeable future.

`package.json`

```
{
    "name": "your-thing"
    ...
    "dependencies": {
        ...
        "elm-loader": "jmackie/elm-loader#0.3.0"
        ...
    }
}
```

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
                            compiler: 'elm',  // use `elm` on PATH
                            debug: true,      // --debug
                            watch: true,      // running in watch mode
                        },
                    },
                ],
            },
        ],
    },

};
```

# Options

#### `compiler`: `string` (default: `'elm'`)

Path to the `elm` compiler binary. This becomes the `command` argument to [`child_prcess.spawn`](https://nodejs.org/api/child_process.html#child_process_child_process_spawn_command_args_options).

#### `debug`: `boolean` (default: `false`)

If set to `true` the `--debug` flag will be passed to `elm make`

#### `optimize`: `boolean` (default: `false`)

If set to `true` the `--optimize` flag will be passed to `elm make`

#### `watch`: `boolean` (default: `false`)

If set to `true` all elm file dependencies will be registered with Webpack.

#### `cwd`: `string` (default: `undefined`)

Optional working directory in which to run.

#### `onCompileBegin`: `function()`

Optional function to be executed before compilation starts. You can use this for logging.

#### `onCompileFinish`: `function()`

Optional function to be executed after compilation finishes. You can use this for logging

[elm-home]: https://elm-lang.org/
[purescript-home]: http://www.purescript.org/
[webpack-home]: https://webpack.js.org/
