import { Component, OnInit } from '@angular/core';

@Component({
	selector: 'app-map',
	templateUrl: './map.component.html',
	styleUrls: ['./map.component.sass']
})
export class MapComponent implements OnInit {

	public static componentName() {
		return 'Map';
	}

	constructor() {}

	ngOnInit(): void {
	}

	called() {
	}

}
