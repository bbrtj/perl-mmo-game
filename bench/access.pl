use all 'Unit';
use header;
use Utils;

use Benchmark::Dumb qw(cmpthese);

my $character = DI->get('faker_service')->fake_character;
my $variables = DI->get('faker_service')->fake_variables;
my $actor = Unit::Actor->new(character => $character, variables => $variables);

cmpthese 200.01, {
	'refaddr' => sub {
		refaddr $actor eq refaddr $actor;
	},
	'serialize' => sub {
		$actor eq $actor;
	},
	'num' => sub {
		$actor == $actor;
	},
	'isa' => sub {
		$actor isa 'Unit::Actor';
	},
	'id' => sub {
		$actor->id eq $actor->id;
	},
};

