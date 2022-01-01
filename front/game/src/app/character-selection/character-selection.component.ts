import { Component, OnInit } from '@angular/core';
import { WSCommunicationService } from '../../websocket/ws-communication.service';
import { Character } from './character';

@Component({
	selector: 'app-character-selection',
	templateUrl: './character-selection.component.html',
	styleUrls: ['./character-selection.component.sass']
})
export class CharacterSelectionComponent implements OnInit {
	characters: Character[] = [];

	public static componentName() {
		return 'CharacterSelection';
	}

	constructor(private ws: WSCommunicationService) {
	}

	ngOnInit(): void {
	}

	async called() {
		this.characters = [];
		this.characters = await this.ws.request('list_characters') as Character[];
	}

}
