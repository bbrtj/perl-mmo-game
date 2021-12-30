import { Component, OnInit } from '@angular/core';
import { WebSocketService } from '../../websocket/websocket.service';

@Component({
	selector: 'app-character-selection',
	templateUrl: './character-selection.component.html',
	styleUrls: ['./character-selection.component.sass']
})
export class CharacterSelectionComponent implements OnInit {

	public static componentName() {
		return 'CharacterSelection';
	}

	constructor(private ws: WebSocketService) {
	}

	ngOnInit(): void {
	}

	called() {
		console.log('called!');
		this.ws.subscribe((data: any) => {
			console.log(data);
		});
		this.ws.send({t: 'list_characters', n: 1});
	}

}
