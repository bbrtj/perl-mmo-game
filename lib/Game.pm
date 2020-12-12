package Game;

use Mojo::Base 'Mojolicious', -signatures;
use Mojo::Pg;
use Game::Common;

# This method will run once at server start
sub startup($self)
{
	my $config = load_config($self);
	load_commands($self, $config);
	load_routes($self, $config);
	load_plugins($self, $config);
	load_helpers($self, $config);
}

sub load_config($self)
{
	my $config = Game::Common->get_config($self, 'vars.conf');

	# Configure the application
	$self->mode($config->{mode} // "development");
	$self->secrets($config->{secrets});

	return $config;
}

sub load_commands($self, $config)
{
	push $self->commands->namespaces->@*, "Game::Command";
}

sub load_routes($self, $config)
{
	my $r = $self->routes;

	# Normal route to controller
	$r->get('/')->to('main#index');
}

sub load_plugins($self, $config)
{
	$self->plugin(Minion => {Pg => $config->{db}{connection}});
}

sub load_helpers($self, $config)
{
	$self->helper(db => sub {
		state $pg = Mojo::Pg->new($config->{db}{connection});
	});
}

1;
