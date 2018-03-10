const path = require('path');

module.exports = {
  mode: "development",
  entry: "./src/main.js",
  output: {
    filename: "bundle.js",
    path: path.join(__dirname, "/build")
  },
  devServer: {
    contentBase: path.resolve(__dirname, 'src')
  },
  resolve: {
        modules: [
            path.resolve(__dirname, "src"),
            "node_modules"
        ],
    },
  module: {
    rules: [{
      test: /\.(html?)$/,
        use: ['html-loader']
      },
    ]
  }
}
