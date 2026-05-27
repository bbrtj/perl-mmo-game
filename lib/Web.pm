package Web;

use My::Moose;
use Path::Tiny;
use Crypt::PRNG qw(random_bytes_b64);
use Web::RedisSession;
use all 'Model';

use header;

extends 'Thunderhorse::App';

around BUILDARGS => sub ($orig, $self, %args) {
	return $self->$orig(
		path => path(__FILE__)->parent->parent,
		initial_config => 'config/web',
		%args,
	);
};

sub build ($self)
{
	DI->get('redis')->connect($self->loop);
}

sub secrets ($self)
{
	my $file = path('secrets');

	if (!$file->exists) {
		$file->spew(random_bytes_b64(64));
	}

	return $file->slurp;
}

sub session_object ($self)
{
	return Web::RedisSession->new;
}

