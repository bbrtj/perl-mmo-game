package Web::Command::seed;

use My::Moose -constr;
use DI;

use header;

extends 'Mojolicious::Command';

use constant description => 'seed test data';
use constant usage => sub ($self) { $self->extract_usage };

sub run ($self, @args)
{
	my $user_service = DI->get('user_service');
	my $faker = DI->get('faker_service');

	for my $email (qw(test test2 test3)) {
		my $user = $user_service->register_user({email => $email . '@test.com', password => 'test'});
		$faker->fake_player($user);
	}
	return;
}

__END__
=head1 SYNOPSIS
	Usage: APPLICATION seed

