var path = require('path');

module.exports = {
    entry: {
        bundle: path.join(__dirname, './src/js/index.js'),
    },
    output: {
        path: path.join(__dirname, 'dist'),
        filename: '[name].js',
    },
    module: {
        rules: [
            {
                test: /\.(elm)$/,
                use: [
                    {
                        loader: path.resolve(__dirname, '../index.js'),
                        options: {
                            debug: false,
                            optimize: true,
                            watch: false,
                            onCompileBegin: () =>
                                console.log('Compiling elm...'),
                            onCompileFinish: () =>
                                console.log('Compilation finished'),
                        },
                    },
                ],
                exclude: /(node_modules)/,
            },
        ],
    },
};
