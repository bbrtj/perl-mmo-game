use all 'Model', 'X', 'Unit', 'Resource', 'Game::Object';
use Game::Helpers;
use Utils;

use header;

use Benchmark::Dumb qw(timethese);

my $ulid = Types::ULID::ulid;
my $character = DI->get('faker_service')->fake_character;
my $variables = DI->get('faker_service')->fake_variables;
my $actor = Unit::Actor->new(character => $character, variables => $variables);

my $pre_effect = Game::Object::Effect::Damage->new(
	actor => $actor,
	lore => lore_ability 'Shoot',
);

timethese 200.01, {
	'Model::Player' => sub {
		Model::Player->new(user_id => $ulid);
	},
	'Model::Character' => sub {
		Model::Character->new(
			player_id => $ulid,
			name => 'DUMMY',
			class_id => 'DUMMY',
			race_id => 'DUMMY',
			alliance_id => 'DUMMY'
		);
	},
	'X::PlayerNotFound' => sub {
		X::PlayerNotFound->new(msg => 'player was not found');
	},
	'Model::PlayerSession' => sub {
		Model::PlayerSession->new();
	},
	'Game::Object::Movement' => sub {
		Game::Object::Movement->new(
			variables => $variables,
			x => 5.3,
			y => 6.5,
			speed => 9,
			time => time,
		);
	},
	'Game::Object::Effect::Damage' => sub {
		Game::Object::Effect::Damage->new(
			actor => $actor,
			lore => lore_ability 'Shoot',
		);
	},
	'Game::Object::Projectile' => sub {
		Game::Object::Projectile->new(
			effect => $pre_effect,
			speed => 0.1,
			angle => 1,
			max_distance => 100,
		);
	},
	'Resource::ActorState' => sub {
		Resource::ActorState->new(subject => $actor);
	},
};

