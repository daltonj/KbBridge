set +H

cd /home/jdalton/kbbridge/
java -server -Dfile.encoding=utf-8 -Xmx4G -classpath /home/jdalton/kbbridge/target/kbbridge-0.1-jar-with-dependencies.jar -Dkbbridge.props=kbbridge-blake.properties edu.umass.ciir.kbbridge.SimpleFeatureExtractor $1
