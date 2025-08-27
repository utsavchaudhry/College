import sys
import torch.nn as nn
from autoencoder import _Autoencoder
from data import Data
from model import Model
from ae1 import AE1

class AE2(_Autoencoder):
    def __init__(self, path):
        super().__init__(path)
        n_kernels = 64
        
        self.encoder = Model(
            input_shape=(self.BATCH_SIZE, n_kernels, 32, 32),
            layers=[
                nn.Conv2d(n_kernels, n_kernels, kernel_size=3, padding=1),
                nn.ReLU(),
                nn.MaxPool2d(kernel_size=2)
            ]
        )
        
        self.decoder = Model(
            input_shape=(self.BATCH_SIZE, n_kernels, 16, 16),
            layers=[
                nn.ConvTranspose2d(n_kernels, n_kernels, kernel_size=3, stride=2, padding=1, output_padding=1),
                nn.ReLU()
            ]
        )
        
        self.model = Model(
            input_shape=self.encoder.input_shape,
            layers=[self.encoder, self.decoder]
        )

if __name__ == '__main__':
    epochs = int(sys.argv[1]) if len(sys.argv) > 1 else None
    data = Data.load('data', image_size=64)
    data.shuffle()
    
    ae1 = AE1('models/ae1.pt')  # Load AE1 to obtain its encoder outputs as training data for AE2
    print(f'\nLoading AE1 from {ae1.path}...')
    ae1.load()
    data_encoded = ae1.encode(data)   # Transform original data through AE1's encoder
    ae2 = AE2('models/ae2.pt')
    ae2.print()
    if not epochs:
        print(f'\nLoading {ae2.path}...')
        ae2.load()
    else:
        # Train AE2 on AE1-encoded data
        print(f'\nTraining AE2 for {epochs} epochs...')
        ae2.train(epochs, data_encoded)
        print(f'\nSaving model to {ae2.path}...')
        ae2.save()
    
    print(f'\nGenerating reconstructed codes with AE2...')
    reconstructed = ae2.generate(data_encoded)
    
    images_reconstructed = ae1.decode(reconstructed)
    
    data.display(16)
    images_reconstructed.display(16)
