package Game::Lore::Npc;

use My::Moose;

use header;

extends 'Game::Lore::Class';

use constant prefix => 'npc';

has param 'level' => (
	isa => PositiveInt,
);

has param 'respawn_time' => (
	isa => PositiveInt,
);

has option 'ai' => (
	isa => Str,
);

has param 'ai_args' => (
	isa => HashRef,
	default => sub { {} },
);

