#!/usr/bin/env ruby -w

proj_root = File.expand_path(File.dirname(__FILE__))

checkout_dirs = Dir['checkouts/*'].map{ |sub_dir| proj_root + "/" + sub_dir}

puts "Pulling dependencies for project root (#{proj_root})..."
`cd #{proj_root} ; lein deps`
if `cd #{proj_root} ; lein help`.match(/native-deps/)
  `cd #{proj_root} ; lein native-deps`
end

puts "Pulling and merging dependencies for checkouts:"
checkout_dirs.each do |dir|
  puts "  - for #{dir}"
  `cd #{dir} ; lein deps`
  if `cd #{dir} ; lein help`.match(/native-deps/)
    `cd #{dir} ; lein native-deps`
  end
  `cd #{dir} ; cp -R #{dir}/lib/* #{proj_root}/lib`
  if(File.exists?("#{dir}/native"))
    `cp -R #{dir}/native #{proj_root}`
  end
end
