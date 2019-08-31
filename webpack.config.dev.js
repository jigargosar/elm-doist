const path = require('path')
const HtmlWebpackPlugin = require('html-webpack-plugin')

// https://webpack.js.org/configuration/
module.exports = {
  mode: 'development',
  entry: { index: './src/index.js' },
  output: {
    publicPath: '/',
  },
  resolve: {
    extensions: ['.js', '.elm'],
  },
  module: {
    rules: [
      // { test: /\.js/, loader: ['cache-loader'] },
      { test: /\.css$/, loader: ['style-loader', 'css-loader'] },
      {
        test: /\.elm$/,
        use: [
          {
            loader: 'elm-webpack-loader',
            options: { optimize: false, debug: true },
          },
        ],
      },
    ],
  },
  plugins: [new HtmlWebpackPlugin({ template: './src/index.html' })],
  // optimization: {
  //   splitChunks: {
  //     cacheGroups: {
  //       commons: {
  //         test: /[\\/]node_modules[\\/]/,
  //         name: 'vendors',
  //         chunks: 'all',
  //       },
  //     },
  //   },
  // },
  // https://webpack.js.org/configuration/stats/
  // stats: 'errors-warnings',
  stats: {
    children: false,
    modules: false,
  },
  // devtool: isProduction ? 'source-map' : 'eval-source-map',
  // devtool: isProduction ? 'source-map' : false,
  devtool: false,
  // https://webpack.js.org/configuration/dev-server/
  devServer: {
    historyApiFallback: true,
    overlay: {
      warnings: true,
      errors: true,
    },
    hot: false,
    // hotOnly: true,
  },
}
