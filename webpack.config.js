module.exports = {
        entry: {
                server: './src/Server/Main.purs'
                // ,
                // im: './src/Client/IM/Main.purs',
        },
        module: {
                rules: [
                  {
                    test: /\.purs$/,
                    use: [
                      {
                        loader: 'purs-loader',
                        options: {
                          src: [
                            'bower_components/purescript-*/src/**/*.purs',
                            'src/**/*.purs'
                          ],
                          bundle: false,
                          watch: true,
                          pscIde: false
                        }
                      }
                    ]
                  },
                ]
              },

              resolve: {
                modules: [ 'node_modules', 'bower_components' ],
                extensions: [ '.purs', '.js']
              }
};