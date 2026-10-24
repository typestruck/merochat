import { main } from './output/Client.Lynx.Main/index.js'
import './file/default/theme-switcher.js'

import './src/Client/css/base.css';
import './src/Client/css/im.css';
import './src/Client/css/profile.css';
import './src/Client/css/landing.css';
import './src/Client/css/help.css';
import './src/Client/css/backer.css';
import './src/Client/css/lynx.css';

main();

if (import.meta.webpackHot) {
  import.meta.webpackHot.accept()
}
