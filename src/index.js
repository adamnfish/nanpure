import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    fixedCells: [
      [[1, 0], 0],
      [[4, 2], 3],
      [[2, 0], 5],
      [[5, 7], 4],
      [[3, 8], 6],
    ]
  }
});

registerServiceWorker();
