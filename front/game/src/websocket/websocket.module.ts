import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

@NgModule({
	declarations: [],
	imports: [
		CommonModule
	]
})
export class WebsocketModule { }

// let socket;
// let subscribers = [];

// function ws_open(event)
// {
// 	console.log("ws subscribed");
// }

// function ws_close(event)
// {
// 	if (!event.wasClean) {
// 		console.error("ws connection terminated");
// 	}
// }

// function ws_error(error)
// {
// 	console.error(error);
// }

// function ws_message(event)
// {
// 	for (let subscriber of subscribers) {
// 		subscriber(JSON.parse(event.data));
// 	}
// }

// export function init()
// {
// 	if (socket === undefined) {
// 		socket = new WebSocket("ws://" + window.location.host + "/subscribe");
// 		socket.onopen = ws_open;
// 		socket.onclose = ws_close;
// 		socket.onmessage = ws_message;
// 		socket.onerror = ws_error;
// 	} else {
// 		console.warn("websocket already initialized");
// 	}
// }

// export function subscribe(subscriber)
// {
// 	if (typeof subscriber === "function") {
// 		subscribers.push(subscriber);
// 	} else {
// 		throw new Error("ws subscriber needs to be a function");
// 	}
// }

// export function send(data)
// {
// 	socket.send(JSON.stringify(data));
// }
