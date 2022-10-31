package Game::Mechanics::Check;

use My::Moose;
use X::CheckFailed;

use header;

has 'error' => (
	is => 'ro',
	predicate => 'has_error',
);

# easy checking of a boolean value
sub check ($self, $message, $check)
{
	return Game::Mechanics::Check->new(
		$check ? () : (error => $message),
	);
}

# complex checking of nested checks and coderefs
sub gather ($self, $message, @checks)
{
	croak "no checks for $message" if @checks == 0;

	foreach my $check (@checks) {
		if (is_coderef $check) {
			$check = $check->();
		}

		if (blessed $check) {
			die "check for $message is blessed but not Game::Mechanics::Check"
				unless $check isa 'Game::Mechanics::Check';

			return $check
				if $check->has_error;
		}
		elsif (!$check) {
			return Game::Mechanics::Check->new(
				error => $message,
			);
		}

	}

	return Game::Mechanics::Check->new;
}

sub result ($self)
{
	return !$self->has_error;
}

sub assert_valid ($self)
{
	if ($self->has_error) {
		X::CheckFailed->throw(msg => $self->error);
	}

	return;
}

