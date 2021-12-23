import { Component, OnInit } from '@angular/core';

@Component({
	selector: 'app-character-selection',
	templateUrl: './character-selection.component.html',
	styleUrls: ['./character-selection.component.sass']
})
export class CharacterSelectionComponent implements OnInit {

	public static componentName() {
		return 'CharacterSelection';
	}

	constructor() {}

	ngOnInit(): void {
	}

}
