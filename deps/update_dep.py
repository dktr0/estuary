#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p "python3.withPackages(ps: [])"

import argparse
import json
import os
import subprocess
import textwrap
import urllib.request

def prefetch_github(owner, repo, rev):
  # nix-prefetch-url --unpack https://github.com/<owner>/<repo>/archive/<rev>.tar.gz
  remote_url = 'https://github.com/%s/%s/archive/%s.tar.gz' % (owner, repo, rev)
  try:
    process = subprocess.run(['nix-prefetch-url', '--unpack', remote_url], check=True, capture_output=True)
    return process.stdout.strip().decode('ascii')
  except subprocess.CalledProcessError:
    print('Could not fetch "%s".' % remote_url)
    return None

def get_latest_commit(owner, repo, branch):
  # GET https://api.github.com/repos/<owner>/<repo>/commits/<branch>
  api_url = 'https://api.github.com/repos/%s/%s/commits/%s' % (owner, repo, branch)
  with urllib.request.urlopen(api_url) as response:
    result = json.load(response)
    sha = result['sha']
    msg = result['commit']['message']
    print('Using rev %s: %s' % (sha, msg))
    return sha

def main():
  parser = argparse.ArgumentParser(
      prog='update_dep',
      description='Update a github dependency.',
      formatter_class=argparse.RawDescriptionHelpFormatter,
      epilog=textwrap.dedent('''
        example:

        Use the latest commit to master:
        ./update_dep.py punctual

        Use the latest commit to dev:
        ./update_dep.py punctual --branch dev

        Use a specific commit:
        ./update_dep.py punctual --rev ab0ba53310d6f764bf19490f835623c915a13eb8
        
        Use a fork:
        ./update_dep.py punctual --rev ab0ba53310d6f764bf19490f835623c915a13eb8 --owner dktr0

        Create a new dep:
        ./update_dep.py punctual --owner dktr0 --repo Punctual
      '''))
  parser.add_argument('name', type=str, help='The dependency name.')
  parser.add_argument('--owner', required=False, type=str, help='The repository owner. Defaults to the one defined in the github.json.')
  parser.add_argument('--repo', required=False, type=str, help='The repository name. Defaults to the one defined in the github.json.')
  parser.add_argument('--rev', required=False, type=str, help='The commit hash.')
  parser.add_argument('--branch', required=False, type=str, help='The branch to fetch last commit from. Defaults to master if --rev not present.')

  args = parser.parse_args()
  dep_folder = os.path.join(os.path.dirname(os.path.realpath(__file__)), args.name)
  if not os.path.isdir(dep_folder):
    os.makedirs(dep_folder, exist_ok=True)

    default_nix_path = os.path.join(dep_folder, 'default.nix')
    print('Creating %s...' % default_nix_path)
    with open(default_nix_path, 'w') as f:
      f.write(textwrap.dedent('''
        import ../github-dep.nix {
          spec = ./github.json;
        }
      ''').strip())
  
  owner = args.owner
  repo = args.repo
  github_json_path = os.path.join(dep_folder, 'github.json')
  if owner is None or repo is None:
    # existing github.json must exist
    if not os.path.isfile(github_json_path):
      print('--owner and --repo must be specified if github.json does not yet exist')
      return

    with open(github_json_path, 'r') as f:
      github_json = json.load(f)
      if owner is None: 
        owner = github_json['owner']
      if repo is None: 
        repo = github_json['repo']

  rev = args.rev
  if rev is None:
    branch = args.branch if args.branch is not None else 'master'
    rev = get_latest_commit(owner, repo, branch)

  sha = prefetch_github(owner, repo, rev)
  if sha is None:
    return
    
  github_json = {
    'owner': owner,
    'repo': repo,
    'rev': rev,
    'sha256': sha
  }

  with open(github_json_path, 'w') as f:
    json.dump(github_json, f, indent=2, sort_keys=True)
    print('Updated "%s".' % github_json_path)

if __name__ == '__main__':
  main()