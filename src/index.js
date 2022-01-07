import './main.css';
import { Elm } from './Picshare.elm';
import * as serviceWorker from './serviceWorker';

const app = Elm.Picshare.init({
  node: document.getElementById('root')
});

const listen = (url) => {
  const socket = new WebSocket(url)

  // WebSocketからデータを受け取ったElmのコードに渡す
  socket.onmessage = (event) => {
    app.ports.receive.send(event.data)
  }
}

// Elmからデータが渡ってくるのを待ち受ける
app.ports.listen.subscribe(listen)

serviceWorker.unregister();
