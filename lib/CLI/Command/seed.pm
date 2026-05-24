package CLI::Command::seed;

use My::Moose;

use header;

BEGIN { extends 'CLI::Command' }

use constant description => 'seed test data';
use constant usage => __PACKAGE__->extract_usage;

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

