package CLI::Command::export_config;

use My::Moose;
use Server::Config;
use Game::Config;

use Path::Tiny qw(cwd);
use JSON::MaybeXS qw(encode_json);

use header;

BEGIN { extends 'CLI::Command' }

use constant description => 'Exports all server config needed by the client';
use constant usage => __PACKAGE__->extract_usage;

sub run ($self)
{
	my $struct = {
		NetworkSeparatorCharacter => Server::Config->PROTOCOL_SEPARATOR,
		NetworkControlCharacter => Server::Config->PROTOCOL_CONTROL_CHARACTER,
		NetworkMaxLength => Server::Config->PROTOCOL_MAX_LENGTH,
		NetworkPort => Server::Config->GAME_SERVER_PORT,
		NetworkTransportFloatPrecision => Server::Config->TRANSPORT_FLOAT_PRECISION,
	};

	my $output = cwd->child('client')->child('data')->child('config.json');
	$output->spew(encode_json $struct);

	say "done, generated in $output";

	return;
}

__END__

=head1 SYNOPSIS

	Usage: APPLICATION export-config

