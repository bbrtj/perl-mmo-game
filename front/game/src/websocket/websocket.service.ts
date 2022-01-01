import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root',
})
export class WebSocketService {
	socket: WebSocket;
	subscribers: Function[] = [];
	opened: boolean = false;

	ws_open(event: any) {
		this.opened = true;
	}

	ws_close(event: any)
	{
		if (!event.wasClean) {
			console.error("ws connection terminated abnormally");
		}
		this.opened = false;
		// TODO: try to reopen
	}

	ws_error(error: any)
	{
		console.error('Websocket error occured:', error);
	}

	ws_message(event: any)
	{
		for (let subscriber of this.subscribers) {
			subscriber(JSON.parse(event.data));
		}
	}

	constructor() {
		this.socket = new WebSocket("ws://" + window.location.host + "/api/socket");
		this.socket.onopen = (e: any) => this.ws_open(e);
		this.socket.onclose = (e: any) => this.ws_close(e);
		this.socket.onmessage = (e: any) => this.ws_message(e);
		this.socket.onerror = (e: any) => this.ws_error(e);
	}

	subscribe(subscriber: Function)
	{
		this.subscribers.push(subscriber);
	}

	send(data: any)
	{
		if (this.opened) {
			this.socket.send(JSON.stringify(data));
		} else {
			setTimeout(() => {
				this.send(data);
			}, 200);
		}
	}
}

