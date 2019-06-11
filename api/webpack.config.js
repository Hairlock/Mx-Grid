const path = require('path');
const webpack = require('webpack');
const slsw = require('serverless-webpack');

const config = {
    resolve: {
        alias: {
            'pg-native': path.join(__dirname, 'aliases/pg-native.js'),
            'pgpass$': path.join(__dirname, 'aliases/pgpass.js'),
          }
    },
    entry: slsw.lib.entries,
    target: 'node', // Ignores built-in modules like path, fs, etc.

    output: {
        libraryTarget: 'commonjs',
        path: path.resolve(`${__dirname}/.webpack`),
        filename: '[name].js',
    },

    module: {
        loaders: [{
            test: /\.elm$/,
            exclude: [/elm-stuff/, /node_modules/],
            loader: 'elm-webpack-loader',
        }],
    },
};

if (process.env.NODE_ENV === 'production') {
    config.module.loaders.push({
        test: /\.js$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: 'babel-loader',
        options: { presets: 'env' },
    });

    config.plugins = config.plugins || [];
    // config.plugins.push(new webpack.optimize.UglifyJsPlugin());
}

module.exports = config;
