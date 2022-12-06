use lib 'local/lib/perl5';
use lib 'lib-base';
use lib 'lib';

use all 'Unit';
use header;
use Utils;
use Scalar::Util qw(refaddr);

use Benchmark qw(cmpthese);

my $character = DI->get('faker_service')->fake_character;
my $variables = DI->get('faker_service')->fake_variables;
my $actor = Unit::Actor->new(character => $character, variables => $variables);

cmpthese - 2, {
	'refaddr' => sub {
		refaddr $actor eq refaddr $actor;
	},
	'serialize' => sub {
		$actor eq $actor;
	},
	'isa' => sub {
		$actor isa 'Unit::Actor';
	},
	'id' => sub {
		$actor->id eq $actor->id;
	},
};

