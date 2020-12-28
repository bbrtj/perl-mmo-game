package Game::Mechanics::Check::Carry;

use header;
use Moo;
use Scalar::Util qw(blessed);

no header;

has 'result' => (
	is => 'ro',
	required => 1,
);

has 'error' => (
	is => 'ro',
	predicate => 'has_error',
);

sub gather ($self, $message, @checks)
{
	die "no checks for `$message`" unless @checks;

	for my $check (reverse @checks) {
		if (ref $check eq 'CODE') {
			$check = $check->();
		}

		if (blessed $check && $check->isa(Game::Mechanics::Check::Carry::)) {
			if ($check->has_error) {
				return $check;
			}
		}
		else {
			if (!$check) {
				return Game::Mechanics::Check::Carry->new(
					result => 0,
					error => $message,
				);
			}
		}

		return Game::Mechanics::Check::Carry->new(
			result => 1,
		);
	}
}

1;
