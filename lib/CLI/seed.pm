package CLI::seed;

use My::Moose -constr;

use header;

extends 'Mojolicious::Command';

use constant description => 'seed test data';
sub usage ($self) { return $self->extract_usage }

sub run ($self, @args)
{
	my $user_service = DI->get('user_service');
	my $character_service = DI->get('user_service');
	my $faker = DI->get('faker_service');

	foreach (1 .. 3) {
		my $user = $user_service->register_user($faker->fake_user('password')->serialize);
		my $player = $character_service->create_character($user, $faker->fake_character->serialize);
	}
	return;
}

__END__

=head1 SYNOPSIS

	Usage: APPLICATION seed

