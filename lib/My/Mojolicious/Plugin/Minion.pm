package My::Mojolicious::Plugin::Minion;

use My::Moose -constr;
use DI;

use header;

extends 'Mojolicious::Plugin';

sub register ($self, $app, $conf)
{
	push @{$app->commands->namespaces}, 'Minion::Command';
	my $minion = DI->get('worker')->minion->app($app);
	$app->helper(minion => sub { $minion });

	return;
}

