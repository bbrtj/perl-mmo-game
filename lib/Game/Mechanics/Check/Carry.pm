package Game::Mechanics::Check::Carry;

use Moo;
use Game::Exception::CheckFailed;

use header;

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
	croak "no checks for `$message`" unless @checks;

	for my $check (reverse @checks) {
		if (ref $check eq 'CODE') {
			$check = $check->();
		}

		if ($check->$_isa('Game::Mechanics::Check::Carry')) {
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

	# should never get here
	return;
}

sub assert_valid ($self)
{
	if ($self->has_error) {
		Game::Exception::CheckFailed->throw(message => $self->error);
	}

	return;
}

