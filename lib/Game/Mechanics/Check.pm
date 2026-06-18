use experimental 'class';

class Game::Mechanics::Check;

use all 'X';

use header;

field $error :reader :param = undef;

method has_error ()
{
	return defined $error;
}

my $success = __PACKAGE__->new;

# easy checking of a boolean value
sub check ($class, $message, $check)
{
	return $check
		? $success
		: $class->new(error => $message)
		;
}

# complex checking of nested checks and coderefs
sub gather ($class, $message, @checks)
{
	croak "no checks for $message" if @checks == 0;

	foreach my $check (@checks) {
		if (ref $check eq 'CODE') {
			$check = $check->();
		}

		if (blessed $check) {
			die "check for $message is blessed but not Game::Mechanics::Check"
				unless $check isa 'Game::Mechanics::Check';

			return $check
				if $check->has_error;
		}
		elsif (!$check) {
			return $class->new(error => $message);
		}

	}

	return $success;
}

method result ()
{
	return !$error;
}

method assert_valid ()
{
	if ($error) {
		X::Pub::CheckFailed->throw($error);
	}

	return;
}

