const path = require('path')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const { CleanWebpackPlugin } = require('clean-webpack-plugin')
const MiniCssExtractPlugin = require('mini-css-extract-plugin')
const OptimizeCSSAssetsPlugin = require('optimize-css-assets-webpack-plugin')
const TerserJSPlugin = require('terser-webpack-plugin')

// https://webpack.js.org/configuration/
module.exports = {
  mode: 'production',
  entry: { index: './src/index.js' },
  output: {
    publicPath: '/',
    path: path.resolve(__dirname, 'dist'),
  },
  resolve: {
    extensions: ['.js', '.elm'],
  },
  module: {
    rules: [
      {
        test: /\.css$/,
        loader: [MiniCssExtractPlugin.loader, 'css-loader'],
      },
      {
        test: /\.elm$/,
        use: [
          {
            loader: 'elm-webpack-loader',
            options: { optimize: true },
          },
        ],
      },
    ],
  },
  plugins: [
    new CleanWebpackPlugin(),
    new HtmlWebpackPlugin({ template: './src/index.html' }),
    new MiniCssExtractPlugin({
      // Options similar to the same options in webpackOptions.output
      // both options are optional
      // filename: '[name].css',
      // chunkFilename: '[id].css',
    }),
  ],
  optimization: {
    minimizer: [new TerserJSPlugin({}), new OptimizeCSSAssetsPlugin({})],
  },
  // https://webpack.js.org/configuration/stats/
  // stats: 'errors-warnings',
  stats: {
    children: false,
    modules: false,
  },
  // devtool: isProduction ? 'source-map' : 'eval-source-map',
  // devtool: isProduction ? 'source-map' : false,
  devtool: false,
}
