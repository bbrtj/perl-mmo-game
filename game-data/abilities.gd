requires 'attributes';

lore STRIKE => ability 'Strike';

translations pl => {
	name => 'Atak',
};

uses attribute 'Physical';
specify weapon_based => true;
# TODO: duration based on weapon
specify speed_multiplier => 1;

