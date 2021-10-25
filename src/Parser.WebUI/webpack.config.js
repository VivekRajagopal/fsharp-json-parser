// Note this only includes basic configuration for development mode.
// For a more comprehensive configuration check:
// https://github.com/fable-compiler/webpack-config-template

var path = require("path");
const CopyPlugin = require("copy-webpack-plugin");

const isProduction = process.argv.find((v) => v.includes("production"));

module.exports = {
  mode: isProduction ? "production" : "development",
  entry: "./src/App.fs.js",
  output: {
    path: path.join(__dirname, "./dist"),
    publicPath: "/",
    filename: "[name].js"
  },
  devServer: {
    publicPath: "/",
    contentBase: "./public",
    port: 8080
  },
  module: {},
  plugins: [
    new CopyPlugin({
      patterns: [{ from: "public", to: "./" }]
    })
  ]
};
