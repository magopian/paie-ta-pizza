import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

const sessionData = localStorage.getItem("session");
const newPizzaData = localStorage.getItem("newPizza");
const serverURL = localStorage.getItem("serverURL") || "";

const app = Elm.Main.init({
	node: document.getElementById("root"),
	flags: { sessionData, newPizzaData, serverURL },
});

app.ports.saveSession.subscribe((sessionData) => {
	localStorage.setItem("session", JSON.stringify(sessionData));
});

app.ports.logoutSession.subscribe(() => {
	localStorage.removeItem("session");
	localStorage.removeItem("newPizza");
});

app.ports.savePizza.subscribe((newPizza) => {
	localStorage.setItem("newPizza", JSON.stringify(newPizza));
});

app.ports.saveServerURL.subscribe((serverURL) => {
	localStorage.setItem("serverURL", serverURL);
});

registerServiceWorker();
