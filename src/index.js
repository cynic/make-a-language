import { Elm } from "./Main.elm";

// let item;
// try {
//   item = JSON.parse(localStorage.getItem('lang-graph'));
// } catch {
//   item = null;
// }
const flags =
    { width : document.getElementsByTagName('html')[0].clientWidth
    , height : document.getElementsByTagName('html')[0].clientHeight
    };
const app = Elm.Main.init({ flags: flags, node: document.getElementById('elm') });
// app.ports.save.subscribe(v => localStorage.setItem('lang-graph', JSON.stringify(v)));