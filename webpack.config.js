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
      }
      ,{
        test: /\.(css|sass|scss)$/,
        use: [
          'style-loader',
          'css-loader',
          {
            loader: 'postcss-loader',
            options: {
              plugins: function () {return [require('precss'), require('autoprefixer')];}
            }
          },
          'sass-loader',]
      },{
        test: /\.js$/,
        enforce: 'post',
        exclude: /node_modules/,
        use: ['babel-loader']
      },
    ]
  }
}
