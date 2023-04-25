package TestHelpers;

use Exporter qw(import);
use Digest::MD5 qw(md5_hex);

use header;

our @EXPORT = qw(
	hash_password
);

# hash password the same way as client would before sending it
sub hash_password ($password)
{
	return md5_hex($password);
}

