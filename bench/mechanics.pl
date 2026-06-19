use Game::Helpers;
use Game::Mechanics::Check::Map qw(can_see);
use Game::Mechanics::Movement qw(move_actor move_projectile);
use Game::Mechanics::Character::Statistics qw(get_current_level get_max_health);
use Game::Mechanics::Combat qw(is_friendly);
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
		die unless can_see($map, [4.5, 3.8], [7.9, 8.3])->result;
	},
	movement => sub {
		die unless move_actor($movement, $map, time);
	},
	projectile => sub {
		die unless move_projectile($projectile, $map, time);
	},
	level => sub {
		die unless get_current_level(250) == 3;
	},
	health => sub {
		die unless get_max_health($class, $stats) == 80 * 1.6 * 1.5;
	},
	friendly => sub {
		die unless is_friendly($actor, $actor);
	},
};

