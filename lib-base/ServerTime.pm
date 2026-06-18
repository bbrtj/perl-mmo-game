package ServerTime;

use v5.42;

use Exporter qw(import);
use Time::HiRes;

our @EXPORT_OK = qw(new_tick mock_server_time server_time);

my $time = Time::HiRes::time;

sub new_tick ()
{
	$time = Time::HiRes::time;
	return;
}

sub mock_server_time ($value)
{
	$time = $value;
	return;
}

sub server_time :prototype()
{
	return $time;
}

