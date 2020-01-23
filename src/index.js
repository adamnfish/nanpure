import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    fixedCells: [
      [[6, 0], 5],
      [[1, 1], 8],
      [[2, 1], 5],
      [[5, 1], 1],
      [[6, 1], 3],
      [[0, 2], 4],
      [[7, 2], 1],
      [[8, 2], 8],
      [[1, 3], 0],
      [[2, 3], 1],
      [[3, 3], 2],
      [[4, 3], 4],
      [[7, 3], 6],
      [[2, 4], 6],
      [[3, 4], 1],
      [[5, 4], 7],
      [[0, 5], 8],
      [[2, 5], 7],
      [[4, 5], 0],
      [[5, 5], 5],
      [[3, 6], 4],
      [[4, 6], 2],
      [[5, 6], 6],
      [[7, 6], 5],
      [[0, 7], 7],
      [[5, 7], 8],
      [[7, 7], 4],
      [[1, 8], 3],
      [[3, 8], 5],
      [[6, 8], 2],
    ]
  }
});

registerServiceWorker();
