use Game::Helpers;
use Game::Checks::Map qw(can_see);
use Game::Mechanics::Movement qw(move_actor move_projectile);
use Game::Mechanics::Character::Statistics qw(get_current_level get_max_health);
use Game::Mechanics::Combat qw(is_friendly);
use Game::Mechanics::Rng qw(random_int random_choice weighted_choice);
use all 'Game::Object', 'Unit';

use header;

use Benchmark::Dumb qw(timethese);

my $location = Game::Lore::Location->new(id => 'TEST', name => 'test', map => 'test_map');
my $map = $location->map;

my $character = DI->get('faker_service')->fake_character;
my $variables = DI->get('faker_service')->fake_variables;
my $actor = Unit::Actor->new(character => $character, variables => $variables);

my $movement = Game::Object::Movement->new(
	variables => $variables,
	x => $variables->pos_x + 20,
	y => $variables->pos_y + 20,
	speed => 0.1,
	time => time,
);

my $effect = Game::Object::Effect::Damage->new(
	actor => $actor,
	lore => lore_ability 'Shoot',
);

my $projectile = Game::Object::Projectile->new(
	effect => $effect,
	speed => 0.1,
	angle => 1,
	max_distance => 100,
);

my $class = lore_class 'Warden';
my $stats = {
	'sstat.stam' => 10,
	'pstat.con' => 10,
};

timethese 200.01, {
	line_of_sight => sub {
		can_see($map, 4.5, 3.8, 7.9, 8.3);
	},
	line_of_sight_fail => sub {
		try {
			can_see($map, 4.5, 3.8, 9.3, 9.6);
		}
		catch ($e) { }
	},
	movement => sub {
		move_actor($movement, $map, time);
	},
	projectile => sub {
		move_projectile($projectile, $map, time);
	},
	level => sub {
		get_current_level(250) == 3;
	},
	health => sub {
		get_max_health($class, $stats) == 80 * 1.6 * 1.5;
	},
	friendly => sub {
		is_friendly($actor, $actor);
	},
	random_int => sub {
		random_int 20, 30;
	},
	random_choice => sub {
		random_choice([1, 2, 3, 4]);
	},
	weighted_choice => sub {
		weighted_choice([[1, 1], [2, 2], [3, 3], [4, 4]]);
	},
};

