const path = require('path');
const glob = require('glob');
const webpack = require('webpack');

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
            path.resolve(__dirname, "output"),
            "node_modules",
            "bower_components"
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
              plugins: function () {return [require('precss'), require('autoprefixer')({ grid: false })];},
            }
          },
          {
            loader: 'sass-loader',
            options: {
              // includePaths: glob.sync('./node_modules').map((d) => path.join(__dirname, d)),
            }
        }]
      },{
        test: /\.js$/,
        enforce: 'post',
        exclude: /node_modules/,
        use: ['babel-loader']
      },{
        test: /\.tag$/,
        enforce: 'pre',
        exclude: /node_modules/,
        use: [{
            loader: 'riot-tag-loader',
            options: {
                debug: true
            }
        }]
      },{
        test: /\.purs$/,
        enforce: 'pre',
        exclude: /node_modules/,
        use: [{
          loader: 'purs-loader',
          options: {
            psc: 'psc',
            src: ['bower_components/purescript-*/src/**/*.purs',
                  'src/purs/*.purs'],
            }
        }]
      }
    ]
  }
}
