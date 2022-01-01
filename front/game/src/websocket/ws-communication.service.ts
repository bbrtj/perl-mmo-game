import { Injectable } from '@angular/core';
import { WebSocketService } from './websocket.service';

@Injectable({
  providedIn: 'root',
})
export class WSCommunicationService {
	private awaiting: any = {};

	private handleEvent(data: any) {
		if ("id" in data) {
			if (data.id in this.awaiting) {
				let id = data.id;
				delete data.id;
				if ("res" in data) {
					this.awaiting[id](data.res);
				}
				else {
					this.awaiting[id](data);
				}
				delete this.awaiting[id];
			} else {
				console.error('unknown websocket event:', data);
			}
		} else {
			// TODO: do something else
			console.log('websocket says:', data);
		}
	}

	private generateId() {
		// id does not have to be globally unique
		// - just unique enough to know which request is which
		let generated;
		do {
			generated = Math.floor(Math.random() * 100);
		} while (generated in this.awaiting);
		return generated;
	}

	constructor (private ws: WebSocketService) {
		this.ws.subscribe((data: any) => {
			this.handleEvent(data);
		});
	}

	async request (type: string, data: any = null) {
		let struct = {
			n: this.generateId(),
			t: type,
			d: data,
		};

		this.ws.send(struct);
		return new Promise((resolve: any, reject: any) => {
			this.awaiting[struct.n] = (returned_data: any) => resolve(returned_data);
			// TODO: reject after a timeout?
		});
	}
}
