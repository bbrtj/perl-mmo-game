requires 'attributes';

# TODO: durations and damages based on weapon

lore STRIKE => ability 'Strike';

translations pl => {
	name => 'Atak',
};

uses attribute 'Physical';
specify speed_multiplier => 1;
specify damage_multiplier => 1;

lore SHOOT => ability 'Shoot';

translations pl => {
	name => 'Strzał',
};

uses attribute 'Physical';
specify speed_multiplier => 1.5;
specify damage_multiplier => 1.5;
specify projectile => {
	speed => 2, # per second
	range => 5,
	radius => 0.01,
	inaccuracy => 15, # degrees
};

