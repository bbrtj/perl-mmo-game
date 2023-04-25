package CLI::seed;

use My::Moose -constr;

use header;

extends 'Mojolicious::Command';

use constant description => 'seed test data';
sub usage ($self) { return $self->extract_usage }

sub run ($self, @args)
{
	my $user_service = DI->get('user_service');
	my $character_service = DI->get('character_service');
	my $faker = DI->get('faker_service');

	my $email = 'test@test.com%s';
	foreach (1 .. 5) {
		my $user = $user_service->register_user(
			{
				$faker->fake_user(email => sprintf($email, $_))->serialize->%*,
				plaintext_password => 'password'
			}
		);
		my $player = $character_service->create_character($user, $faker->fake_character->serialize);
	}
	return;
}

__END__

=head1 SYNOPSIS

	Usage: APPLICATION seed

