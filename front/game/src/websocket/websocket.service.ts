import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root',
})
export class WebSocketService {
	socket: WebSocket;
	subscribers: Function[] = [];

	ws_open(event: any) {
		console.log("ws subscribed");
	}

	ws_close(event: any)
	{
		if (!event.wasClean) {
			console.error("ws connection terminated");
		}
	}

	ws_error(error: any)
	{
		console.error('Websocket error occured:', error);
	}

	ws_message(event: any)
	{
		console.log('got event:', event);
		for (let subscriber of this.subscribers) {
			subscriber(JSON.parse(event.data));
		}
	}

	constructor() {
		this.socket = new WebSocket("ws://" + window.location.host + "/api/socket");
		this.socket.onopen = this.ws_open;
		this.socket.onclose = this.ws_close;
		this.socket.onmessage = this.ws_message;
		this.socket.onerror = this.ws_error;
	}

	subscribe(subscriber: Function)
	{
		this.subscribers.push(subscriber);
	}

	send(data: any)
	{
		this.socket.send(JSON.stringify(data));
	}
}

