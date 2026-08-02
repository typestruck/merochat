import { main } from './output/Client.Im.Main/index.js'

import './src/Client/css/base.css';
import './src/Client/css/im.css';

main();

if (import.meta.webpackHot) {
  import.meta.webpackHot.accept()
}
