import { ComponentRef } from '@angular/core';

export interface ComponentMap {
	[index: string]: ComponentRef<any>,
}
