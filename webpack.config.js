module.exports = {
  module: {
    loaders: [{
      test: /\.elm$/,
      include: [/src/],
      loader: 'elm-webpack'
    }]
  }
}
