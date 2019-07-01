import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {

  },
});

const url = 'ws://localhost:8000';
const connection = new WebSocket(url)
connection.onmessage = e => {
  app.ports.phoneCallIn.send(JSON.parse(e.data))

};




registerServiceWorker();
