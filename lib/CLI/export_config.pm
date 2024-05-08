package CLI::export_config;

use My::Moose -constr;
use Server::Config;
use Game::Config;

use Mojo::File qw(path);
use Mojo::JSON qw(encode_json);

use header;

extends 'Mojolicious::Command';

use constant description => 'Exports all server config needed by the client';
sub usage ($self) { return $self->extract_usage }

sub run ($self)
{
	my $struct = {
		NetworkSeparatorCharacter => Server::Config->PROTOCOL_SEPARATOR,
		NetworkControlCharacter => Server::Config->PROTOCOL_CONTROL_CHARACTER,
		NetworkMaxLength => Server::Config->PROTOCOL_MAX_LENGTH,
		NetworkPort => Server::Config->GAME_SERVER_PORT,

		GameActionCooldown => Game::Config->config->{action_cooldown},
	};

	my $output = path->child('client')->child('data')->child('config.json');
	$output->spurt(encode_json $struct);

	say "done, generated in $output";

	return;
}

__END__

=head1 SYNOPSIS

	Usage: APPLICATION export-config

